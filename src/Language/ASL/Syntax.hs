module Language.ASL.Syntax where

import Data.Text(Text)

-- TODO: what is the real meaning of these Unknown bits?

-- Infrastructure ---------------------------------------------------

type Identifier = Text

data QualifiedIdentifier = QualifiedIdentifier ArchQualifier Identifier

data ArchQualifier = ArchQualAArch32
                   | ArchQualAArch64
                   | ArchQualAny

type SymbolDecl = (Identifier, Type)

data MaskBit = MaskBitSet | MaskBitUnset | MaskBitEither
    deriving(Show, Eq)

type BitVector = [Bool]
type Mask = [MaskBit]

-- Instructions -----------------------------------------------------

data Instruction =
  Instruction { instName      :: Text
              , instEncodings :: [InstructionEncoding]
              , instExecute   :: [Stmt]
              }

data InstructionSet = A32 | T32

data InstructionEncoding =
  InstructionEncoding { encInstrSet      :: InstructionSet
                      , encFields        :: [InstructionField]
                      , encOpcodeMask    :: Mask
                      , encGuard         :: Expr
                      , encUnpredictable :: [(Int, Bool)]
                      , encDecode        :: [Stmt]
                      }

data InstructionField =
  InstructionField { instFieldName   :: Identifier
                   , instFieldBegin  :: Int
                   , instFieldOffset :: Int
                   }

-- Definitions ------------------------------------------------------

data Definition =
    DefTypeBuiltin      Identifier
  | DefTypeAbstract     Identifier
  | DefTypeAlias        Identifier Type
  | DefTypeStruct       QualifiedIdentifier [SymbolDecl]
  | DefTypeEnum         Identifier [Identifier]
  | DefVariable         QualifiedIdentifier Type
  | DefConst            Identifier Type Expr
  | DefArray            Identifier Type IndexType
  | DefCallable         { callableName ::  QualifiedIdentifier
                        , callableArgs ::  [SymbolDecl]
                        , callableRets ::  [Type]
                        , callableStmts :: [Stmt]
                        }
  | DefGetter           QualifiedIdentifier [SymbolDecl] [Type] [Stmt]
  | DefSetter           QualifiedIdentifier [SetterArg] SymbolDecl [Stmt]

data SetterArg = SetterArg SymbolDecl Bool

-- Types ------------------------------------------------------------

data Type =
    TypeRef   QualifiedIdentifier
  | TypeFun   Identifier Expr
  | TypeOf    Expr
  | TypeReg   Integer [RegField]
  | TypeArray Type IndexType

data IndexType =
    IxTypeRange Expr Expr
  | IxTypeRef   Identifier

data RegField = RegField Identifier [Slice]

-- Statements -------------------------------------------------------

data LValExpr =
    LValIgnore
  | LValVarRef QualifiedIdentifier
  | LValMember LValExpr Identifier
  | LValDotIndex LValExpr [Identifier]
  | LValIndex LValExpr [Slice]
  | LValArrayPattern [LValExpr]
  | LValList [LValExpr]

data Stmt =
    StmtDecl Type [Identifier]
  | StmtDeclInit  SymbolDecl Expr
  | StmtDeclConst SymbolDecl Expr
  | StmtAssign LValExpr Expr
  | StmtCall QualifiedIdentifier [Expr]
  | StmtReturn (Maybe Expr)
  | StmtAssert Expr
  | StmtUnpredictable
  | StmtImpDef String
  | StmtIfThen [(Expr, [Stmt])] (Maybe [Stmt])
  | StmtCase Expr [CaseAlternative]
  | StmtFor Identifier (Expr, Expr) [Stmt]
  | StmtWhile Expr [Stmt]
  | StmtRepeat [Stmt] Expr
  | StmtThrow Identifier
  | StmtUndefined
  | StmtSeeExpr Expr
  | StmtSeeString String
  | StmtTry [Stmt] Identifier [CatchAlternative]

data CaseAlternative =
    CaseWhen [CasePattern] (Maybe Expr) [Stmt]
  | CaseOtherwise [Stmt]

data CasePattern =
    CasePatternInt Integer
  | CasePatternHex Integer
  | CasePatternBin BitVector
  | CasePatternMask Mask
  | CasePatternIdentifier Identifier
  | CasePatternIgnore
  | CasePatternSeq [CasePattern]

data CatchAlternative =
    CatchWhen Expr [Stmt]
  | CatchOtherwise [Stmt]

-- Expressions ------------------------------------------------------

data Expr =
    ExprLitString Text
  | ExprLitInt Integer
  | ExprLitReal Rational
  | ExprLitBin BitVector
  | ExprLitMask Mask
  | ExprVarRef Identifier
  | ExprImpDef (Maybe Text)
  | ExprSlice  [Slice]
  | ExprConcat Expr Expr
  | ExprIndex Expr [Slice]
  | ExprUnOp UnOp Expr
  | ExprBinOp BinOp Expr Expr
  | ExprMembers Expr [Identifier]
  | ExprInMask Expr Mask
  | ExprMemberBits Expr [Identifier]
  | ExprCall Expr [Expr]
  | ExprInSet Expr Expr [SetElement]
  | ExprUnknown
  | ExprTuple [Expr]
  | ExprIf {- test, result -} [(Expr, Expr)]
           {- else -}Expr
  | ExprMember Expr Identifier

data SetElement =
    SetEltSingle Expr
  | SetEltRange Expr Expr

data Slice =
    SliceSingle Expr
  | SliceOffset Expr Expr

data UnOp =
    UnOpNot
  | UnOpNeg

data BinOp =
    BinOpEQ
  | BinOpNEQ
  | BinOpGT
  | BinOpGTEQ
  | BinOpShiftRight
  | BinOpLT
  | BinOpLTEQ
  | BinOpShiftLeft
  | BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDivide -- this is the '/' operator?
  | BinOpPow
  | BinOpLogicalAnd
  | BinOpLogicalOr
  | BinOpBitwiseOr
  | BinOpBitwiseAnd
  | BinOpBitwiseXor
  | BinOpPlusPlus   -- this is the '++' operator - doesn't appear in out.asl
  | BinOpQuot
  | BinOpRem
  | BinOpDiv
  | BinOpMod
