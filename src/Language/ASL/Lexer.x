{

{-# OPTIONS -w #-}

module Language.ASL.Lexer
( Token(..)
, LocatedToken(..)
, tokenize
) where

import Prelude hiding(lex)
import Control.Monad
import Language.ASL.Syntax(ArchName(..), MaskBit(..))
import Numeric
}

%wrapper "posn"

$space = [\ \t\n]
$newline = \n
$all = [.\n]

tokens :-
  $space+                 ;
  "//".*                  ;
  "/*" $all* "*/"         ;

-- Keywords
  "AND"                     { lex' TokenAnd           }
  "DIV"                     { lex' TokenDiv           }
  "EOR"                     { lex' TokenEor           }
  "IMPLEMENTATION_DEFINED"  { lex' TokenImpDef        }
  "IN"                      { lex' TokenIn            }
  "MOD"                     { lex' TokenMod           }
  "NOT"                     { lex' TokenNot           }
  "OR"                      { lex' TokenOr            }
  "QUOT"                    { lex' TokenQuot          }
  "REM"                     { lex' TokenRem           }
  "SEE"                     { lex' TokenSee           }
  "UNDEFINED"               { lex' TokenUndefined     }
  "UNKNOWN"                 { lex' TokenUnknown       }
  "UNPREDICTABLE"           { lex' TokenUnpredictable }
  "__builtin"               { lex' TokenBuiltin       }
  "__register"              { lex' TokenRegister      }
  "array"                   { lex' TokenArray         }
  "assert"                  { lex' TokenAssert        }
  "case"                    { lex' TokenCase          }
  "catch"                   { lex' TokenCatch         }
  "constant"                { lex' TokenConstant      }
  "do"                      { lex' TokenDo            }
  "downto"                  { lex' TokenDownTo        }
  "else"                    { lex' TokenElse          }
  "elsif"                   { lex' TokenElsIf         }
  "enumeration"             { lex' TokenEnum          }
  "for"                     { lex' TokenFor           }
  "if"                      { lex' TokenIf            }
  "is"                      { lex' TokenIs            }
  "of"                      { lex' TokenOf            }
  "otherwise"               { lex' TokenOtherwise     }
  "repeat"                  { lex' TokenRepeat        }
  "return"                  { lex' TokenReturn        }
  "then"                    { lex' TokenThen          }
  "throw"                   { lex' TokenThrow         }
  "to"                      { lex' TokenTo            }
  "try"                     { lex' TokenTry           }
  "type"                    { lex' TokenType          }
  "typeof"                  { lex' TokenTypeOf        }
  "until"                   { lex' TokenUntil         }
  "when"                    { lex' TokenWhen          }
  "while"                   { lex' TokenWhile         }

-- Numbers, Strings, Identifiers

  \" [^\"]* \"              { lex lexStringLit        }
  ' [01 ]* '                { lex lexBv               }
  ' [01 x]* '               { lex lexMask             }
  "0x" [0-9A-Fa-f_]+        { lex lexHex              }
  [0-9]+ \. [0-9]+          { lex lexReal             }
  [0-9]+                    { lex (TokenInt . read)   }

  "AArch32"                 { lex' (TokenQualifier AArch32) }
  "AArch64"                 { lex' (TokenQualifier AArch64) }

  [a-zA-Z_][a-zA-Z0-9_]*    { lex TokenIdent          }

-- Punctuation
  "!"                       { lex' TokenBang          }
  "!="                      { lex' TokenNeq           }
  "&&"                      { lex' TokenAmpAmp        }
  "&"                       { lex' TokenAmp           }
  "("                       { lex' TokenLParen        }
  ")"                       { lex' TokenRParen        }
  "*"                       { lex' TokenStar          }
  "+"                       { lex' TokenPlus          }
  "++"                      { lex' TokenPlusPlus      }
  "+:"                      { lex' TokenPlusColon     }
  ","                       { lex' TokenComma         }
  "-"                       { lex' TokenMinus         }
  "."                       { lex' TokenDot           }
  ".."                      { lex' TokenDotDot        }
  "/"                       { lex' TokenSlash         }
  ":"                       { lex' TokenColon         }
  ";"                       { lex' TokenSemi          }
  "<"                       { lex' TokenLt            }
  "<<"                      { lex' TokenLtLt          }
  "<="                      { lex' TokenLtEq          }
  "="                       { lex' TokenEq            }
  "=="                      { lex' TokenEqEq          }
  ">"                       { lex' TokenGt            }
  ">="                      { lex' TokenGtEq          }
  ">>"                      { lex' TokenGtGt          }
  "["                       { lex' TokenLBrack        }
  "]"                       { lex' TokenRBrack        }
  "^"                       { lex' TokenCaret         }
  "{"                       { lex' TokenLBrace        }
  "||"                      { lex' TokenBarBar        }
  "}"                       { lex' TokenRBrace        }

{

lex :: (String -> Token) -> AlexPosn -> String -> LocatedToken
lex f (AlexPn _ line col) s = LTk { ltLocation = (line, col)
                                  , ltToken = f s
                                  }

lex' :: Token -> AlexPosn -> String -> LocatedToken
lex' = lex . const

data LocatedToken = LTk { ltLocation :: (Int, Int)
                        , ltToken    :: Token
                        }

data Token =
-- Keywords:
    TokenIndent
  | TokenUnindent
  | TokenNewline
  | TokenAnd
  | TokenDiv
  | TokenEor
  | TokenImpDef
  | TokenIn
  | TokenMod
  | TokenNot
  | TokenOr
  | TokenQuot
  | TokenRem
  | TokenSee
  | TokenUndefined
  | TokenUnknown
  | TokenUnpredictable
  | TokenBuiltin
  | TokenRegister
  | TokenArray
  | TokenAssert
  | TokenCase
  | TokenCatch
  | TokenConstant
  | TokenDo
  | TokenDownTo
  | TokenElse
  | TokenElsIf
  | TokenEnum
  | TokenFor
  | TokenIf
  | TokenIs
  | TokenOf
  | TokenOtherwise
  | TokenRepeat
  | TokenReturn
  | TokenThen
  | TokenThrow
  | TokenTo
  | TokenTry
  | TokenType
  | TokenTypeOf
  | TokenUntil
  | TokenWhen
  | TokenWhile

-- Numbers, Strings, Identifiers:
  | TokenString String
  | TokenBin [Bool]
  | TokenMask [MaskBit]
  | TokenHex Integer
  | TokenReal Rational
  | TokenInt Integer
  | TokenIdent String
  | TokenQualifier ArchName

-- Punctuation
  | TokenBang
  | TokenNeq
  | TokenAmp
  | TokenAmpAmp
  | TokenLParen
  | TokenRParen
  | TokenStar
  | TokenPlus
  | TokenPlusPlus
  | TokenPlusColon
  | TokenComma
  | TokenMinus
  | TokenDot
  | TokenDotDot
  | TokenSlash
  | TokenColon
  | TokenSemi
  | TokenLt
  | TokenLtLt
  | TokenLtEq
  | TokenEq
  | TokenEqEq
  | TokenGt
  | TokenGtEq
  | TokenGtGt
  | TokenLBrack
  | TokenRBrack
  | TokenCaret
  | TokenLBrace
  | TokenRBrace
  | TokenBarBar
  deriving(Eq, Show)

discardQuotes :: Char -> String -> String
discardQuotes q s =
  let uqBegin = dropWhile (q==) s
  in takeWhile (q/=) uqBegin

lexStringLit :: String -> Token
lexStringLit s = TokenString (discardQuotes '"' s)

lexBv :: String -> Token
lexBv s = TokenBin (asBit <$> s')
  where
    s' = filter (/=' ') (discardQuotes '\'' s)
    asBit '0' = False
    asBit '1' = True
    asBit _   = error "Lexical error: bitvector contains invalid char"

lexMask :: String -> Token
lexMask s = TokenMask (asMaskBit <$> s' )
  where
    s' = filter (/=' ') (discardQuotes '\'' s)
    asMaskBit '0' = MaskBitUnset
    asMaskBit '1' = MaskBitSet
    asMaskBit 'x' = MaskBitEither
    asMaskBit _   = error "Lexical error: mask contains invalid char"

lexHex :: String -> Token
lexHex ('0':'x':s) = TokenHex $
  case readHex (filter (/='_') s) of
    [(i, "")] -> i
    _         -> error "Lexical error: invalid hex literal"
lexHex _ = error "Lexical error: invalid hex literal"

lexReal :: String -> Token
lexReal s = TokenReal $
  case readFloat s of
    [(i, "")] -> i
    _         -> error "Lexical error: invalid real literal"



data IndentCtx = IndentCtx { ctxIndents  :: [Int]
                           , ctxTokens   :: [LocatedToken]
                           , ctxBrackets :: [Token]
                           , ctxLine     :: Int
                           , ctxUnindent :: Bool
                           }

-- addIndents' :: [LocatedToken] -> [LocatedToken]
-- addIndents' ts = foldr go [] ts
--    where
--      contextualized = foldr ctxify [] ts
--      ctxify tok ctxLst =

-- TODO: suspend indent in bracks
addIndents :: [LocatedToken] -> [LocatedToken]
addIndents ts = reverse (doIndent [0] ts [] 0 False)
  where
    doIndent :: [Int] -> [LocatedToken] -> [LocatedToken] -> Int -> Bool -> [LocatedToken]
    doIndent [0] [] acc _ _ = acc
    doIndent (i:is) [] acc ln _ = doIndent is [] (LTk (ln + 1, 0) TokenUnindent : acc) ln False
    doIndent [] _ acc _ _ = error "should not empty indent"
    doIndent ctx@(currentIndent:istack) (tcur:trest) acc ln ui

      -- emit the token as-is
      | isSameLine = doIndent ctx trest (tcur:acc) ln False

      -- indent a level
      | not ui && currentCol > currentIndent =
          doIndent (currentCol:ctx) (tcur:trest) (loc TokenIndent:acc) currentLine False

      -- if we are unindenting and this happens, it is an error
      | ui && col tcur > currentIndent =
          error $ "Bad unindent on line " ++ show currentLine

      -- unindent
      | col tcur < currentIndent =
          doIndent istack (tcur:trest) (loc TokenUnindent:acc) currentLine True

      | otherwise = doIndent ctx trest (tcur:acc) currentLine False
      where
        (currentLine, currentCol) = ltLocation tcur
        col (LTk (_, c) _) = c
        isSameLine = not isNewLine
        isNewLine = ln /= currentLine
        loc i = LTk (currentLine, 0) i

tokenize :: String -> [LocatedToken]
tokenize s = addIndents (alexScanTokens s)

}