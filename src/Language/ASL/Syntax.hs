module Language.ASL.Syntax where


-- TODO: what is the real meaning of these Unknown bits?

-- Infrastructure ---------------------------------------------------

type Identifier = String

data QualifiedIdentifier = QualifiedIdentifier (Maybe ArchName) Identifier

type SymbolDecl = (Identifier, Type)

data ArchName = AArch32 | AArch64
    deriving(Show, Eq)

data MaskBit = MaskBitSet | MaskBitUnset | MaskBitEither
    deriving(Show, Eq)

type BitVector = [Bool]

-- Definitions ------------------------------------------------------

data Definition =
    TypeDefinition      TypeDefinition
  | VariableDefinition  QualifiedIdentifier Type
  | ConstDefinition     Identifier Type Expr
  | ArrayDefinition     Identifier Type IndexType
  | FunctionDefinition  CallableDef
  | ProcedureDefinition CallableDef
  | GetterDefinition    QualifiedIdentifier [SymbolDecl] [Type] [Stmt]
  | SetterDefinition    QualifiedIdentifier [SetterArg] SymbolDecl [Stmt]

data SetterArg = SetterArg SymbolDecl Bool  {- True if AMP - what does this mean? -}

data TypeDefinition =
    TypeDefBuiltin  Identifier
  | TypeDefAbstract Identifier  -- abstract?
  | TypeDefAlias    Identifier Type
  | TypeDefStruct   Identifier [SymbolDecl]
  | TypeDefEnum     Identifier [Identifier]

data CallableDef = CallableDef { callableName ::  QualifiedIdentifier
                               , callableArgs ::  [SymbolDecl]
                               , callableRets ::  [Type]
                               , callableStmts :: [Stmt]
                               }


-- Types ------------------------------------------------------------

data Type =
    TypeRef   Identifier
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
    LValDiscard                        {- UNKNOWN -}
  | LValVarRef QualifiedIdentifier     {- UNKNOWN -}
  | LValMember LValExpr Identifier     {- UNKNOWN -}
  | LValDotIndex LValExpr [Identifier] {- UNKNOWN -}
  | LValIndex LValExpr [Slice]         {- UNKNOWN -}
  | LValArrayPattern [LValExpr]        {- UNKNOWN -}
  | LValList [LValExpr]                {- UNKNOWN -}

data Stmt =
    StmtDecl Type [Identifier]          {- UNKNOWN -}
  | StmtDeclInit  SymbolDecl Expr       {- UNKNOWN -}
  | StmtDeclConst SymbolDecl Expr       {- UNKNOWN -}
  | StmtAssign LValExpr Expr            {- UNKNOWN -}
  | StmtCall QualifiedIdentifier [Expr] {- UNKNOWN -}
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
  | CasePatternMask [MaskBit]
  | CasePatternIdentifier Identifier
  | CasePatternIgnore                    {- UNKNOWN - this corresponds to minus in a pattern -}
  | CasePatternSeq [CasePattern]

data CatchAlternative =
    CatchWhen Expr [Stmt]
  | CatchOtherwise [Stmt]

-- Expressions ------------------------------------------------------

data Expr = Expr
data Slice = Slice

