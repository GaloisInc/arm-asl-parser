{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Language.ASL.Parser where

import Data.Text(Text, pack, unpack)
import Text.Parsec.Text(Parser)
import qualified Text.Parsec.Char as P
import qualified Text.Parsec as P
import Numeric(readFloat, showFFloat)

import Data.SCargot(encodeOne, decodeOne, SExprPrinter(..), SExprParser(..)
                   , basicPrint, setFromCarrier)
import Data.SCargot.Parse( mkParser, asWellFormed )
import Data.SCargot.Repr(WellFormedSExpr(..), fromWellFormed)
import Control.Monad.Except(Except(..), MonadError(..))

import qualified Language.ASL.Syntax as Syn

-- PARSER -----------------------------------------------------------

data Atom = AtomId   Syn.Identifier
          | AtomInt  Integer
          | AtomReal Integer Integer
          | AtomMask Syn.Mask
          | AtomBin  Syn.BitVector
          | AtomString Text
  deriving(Show, Eq)

runParseAtom :: String -> Either P.ParseError Atom
runParseAtom t = P.parse parseAtom "<input>" (pack t)

showAtom :: Atom -> Text
showAtom = \case
    AtomId i -> i
    AtomInt n -> pack $ show n
    AtomReal i f -> pack $ show i <> "." <> show f
    AtomMask m -> "'" <> (pack $ fmap showMaskBit m)  <> "'"
    AtomBin b -> "'" <> (pack $ fmap showBinBit b) <> "'"
    AtomString s -> "\"" <> s <> "\""
  where
    showBinBit = \case
      True -> '1'
      False -> '0'

    showMaskBit = \case
      Syn.MaskBitEither -> 'x'
      Syn.MaskBitSet    -> '1'
      Syn.MaskBitUnset  -> '0'

parseAtom :: Parser Atom
parseAtom = P.choice $ [ parseIdentifier
                       , P.try parseReal
                       , parseInt
                       , P.try parseBin
                       , parseMask
                       ]
  where
    parseIdentifier = do
      l <- P.choice [ P.letter, P.char '_' ]
      r <- P.many $ P.choice [ P.alphaNum, P.char '_' ]
      return (AtomId $ pack (l:r))

    parseString :: Parser Atom
    parseString = do
      _   <- P.char '"'
      str <- P.many (P.noneOf ['"'])
      _   <- P.char '"'
      return (AtomString $ pack str)

    parseInt = do
      n <- P.many1 P.digit
      return (AtomInt $ read n)

    parseReal = do
      i <- P.many1 P.digit
      d <- P.char '.'
      f <- P.many1 P.digit
      return (AtomReal (read i) (read f))

    parseBin = do
      _ <- P.char '\''
      bits <- P.many (P.oneOf "10 ")
      _ <- P.char '\''
      return (AtomBin $ parseBits bits)

    parseMask = do
      _ <- P.char '\''
      bits <- P.many (P.oneOf "10x ")
      _ <- P.char '\''
      return (AtomMask $ parseMaskBits bits)

    parseBit b = b == '1'
    parseBits bs = parseBit <$> (filter (`elem` ['0', '1']) bs)

    parseMaskBit b = case b of
      '0' -> Syn.MaskBitSet
      '1' -> Syn.MaskBitUnset
      'x' -> Syn.MaskBitEither
      _   -> error "invalid mask bit"
    parseMaskBits bs = parseMaskBit <$> (filter (`elem` ['0', '1', 'x']) bs)


type SExprA = WellFormedSExpr Atom

sexprParser :: SExprParser Atom (SExprA)
sexprParser = asWellFormed (mkParser parseAtom)

sexprPrinter :: SExprPrinter Atom (SExprA)
sexprPrinter = setFromCarrier fromWellFormed (basicPrint showAtom)

-- PARSER -----------------------------------------------------------

err :: Text -> Except Text a
err t = throwError t

unexpectedForm :: Text -> SExprA -> Except Text a
unexpectedForm desc expr = err msg
  where
    msg = "in " <> desc <> " : " <> encodeOne sexprPrinter expr

-- tryWith :: Text -> SExprA -> Except [(Text, SExprA)] a -> Except [(Text, SExprA)] a
-- tryWith t s c = c `catchError` \es -> err $ (t,s):err

nameArgs :: SExprA -> Except Text (Syn.Identifier, [SExprA])
nameArgs (WFSList (nmA:argsA)) = do
  nm <- parseId nmA
  return (nm, argsA)
nameArgs s = unexpectedForm "nameArgs" s

parseDefs :: SExprA -> Except Text [Syn.Definition]
parseDefs ds = nameArgs ds >>= \case
  ("AslDefinitions", defs) -> mapM parseDef defs
  _                        -> throwError "Expecting AslDefinitions"

parseDef :: SExprA -> Except Text Syn.Definition
parseDef d = nameArgs d >>= \case
  ("DefTypeBuiltin", [i]) -> do
    name <- parseId i
    return $ Syn.DefTypeBuiltin name

  ("DefTypeAbstract", [i]) -> do
    name <- parseId i
    return $ Syn.DefTypeAbstract name

  ("DefTypeAlias", [i,t]) -> do
    name <- parseId i
    typ <- parseType t
    return $ Syn.DefTypeAlias name typ

  ("DefTypeStruct", [q, sdecls]) -> do
    name <- parseQualId q
    decls <- parseList parseSymDecl sdecls
    return $ Syn.DefTypeStruct name decls

  ("DefTypeEnum", [i, ns]) -> do
    name <- parseId i
    eNames <- parseList parseId ns
    return $ Syn.DefTypeEnum name eNames

  _ -> unexpectedForm "parseDef" d

-- TODO: finish types
parseType :: SExprA -> Except Text Syn.Type
parseType s = nameArgs s >>= \case
  ("TypeRef", [q]) -> do
    qid <- parseQualId q
    return $ Syn.TypeRef qid

  _ -> unexpectedForm "parseType" s

parseSymDecl :: SExprA -> Except Text Syn.SymbolDecl
parseSymDecl s = nameArgs s >>= \case
  ("SymbolDecl", [i, t]) -> do
    ident <- parseId i
    typ <- parseType t
    return (ident, typ)

parseQualId :: SExprA -> Except Text Syn.QualifiedIdentifier
parseQualId s = nameArgs s >>= \case
  ("QualifiedIdentifier", [a, i]) -> do
    archId <- parseId a
    iden <- parseId i
    arch <- case archId of
      "AArch32" -> return Syn.ArchQualAArch32
      "AArch64" -> return Syn.ArchQualAArch64
      "Any"     -> return Syn.ArchQualAny
      _         -> unexpectedForm "parseQualId" s

    return $ Syn.QualifiedIdentifier arch iden

  _ -> unexpectedForm "parseQualId" s

parseMaybe :: (SExprA -> Except Text a) -> SExprA -> Except Text (Maybe a)
parseMaybe f s = case s of
  WFSAtom (AtomId "Nothing")   -> return Nothing
  WFSList [(WFSAtom (AtomId "Just")), k] -> do
    r <- f k
    return $ Just r
  _ -> unexpectedForm "parseMaybe" s

parseList :: (SExprA -> Except Text a) -> SExprA -> Except Text [a]
parseList f l = nameArgs l >>= \case
  ("list", elts) -> mapM f elts
  _              -> unexpectedForm "parseList" l

parseId :: SExprA -> Except Text Syn.Identifier
parseId = \case
  WFSAtom (AtomId i) -> return i
  k                  -> unexpectedForm "parseId" k

