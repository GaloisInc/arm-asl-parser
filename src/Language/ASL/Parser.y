{

module Language.ASL.Parser where

import Language.ASL.Syntax
import Language.ASL.Lexer

import Data.Maybe(isJust)

}

%name parseASL definitions

%tokentype { LocatedToken }
%error { happyError }

%token
  indent                   { LTk _ TokenIndent        }
  unindent                 { LTk _ TokenUnindent      }

  AND                      { LTk _ TokenAnd           }
  DIV                      { LTk _ TokenDiv           }
  EOR                      { LTk _ TokenEor           }
  IMPLEMENTATION_DEFINED   { LTk _ TokenImpDef        }
  IN                       { LTk _ TokenIn            }
  MOD                      { LTk _ TokenMod           }
  NOT                      { LTk _ TokenNot           }
  OR                       { LTk _ TokenOr            }
  QUOT                     { LTk _ TokenQuot          }
  REM                      { LTk _ TokenRem           }
  SEE                      { LTk _ TokenSee           }
  UNDEFINED                { LTk _ TokenUndefined     }
  UNKNOWN                  { LTk _ TokenUnknown       }
  UNPREDICTABLE            { LTk _ TokenUnpredictable }
  builtin                  { LTk _ TokenBuiltin       }
  register                 { LTk _ TokenRegister      }
  array                    { LTk _ TokenArray         }
  assert                   { LTk _ TokenAssert        }
  case                     { LTk _ TokenCase          }
  catch                    { LTk _ TokenCatch         }
  constant                 { LTk _ TokenConstant      }
  do                       { LTk _ TokenDo            }
  downto                   { LTk _ TokenDownTo        }
  else                     { LTk _ TokenElse          }
  elsif                    { LTk _ TokenElsIf         }
  enumeration              { LTk _ TokenEnum          }
  for                      { LTk _ TokenFor           }
  if                       { LTk _ TokenIf            }
  is                       { LTk _ TokenIs            }
  of                       { LTk _ TokenOf            }
  otherwise                { LTk _ TokenOtherwise     }
  repeat                   { LTk _ TokenRepeat        }
  return                   { LTk _ TokenReturn        }
  then                     { LTk _ TokenThen          }
  throw                    { LTk _ TokenThrow         }
  to                       { LTk _ TokenTo            }
  try                      { LTk _ TokenTry           }
  'type'                   { LTk _ TokenType          }
  typeof                   { LTk _ TokenTypeOf        }
  until                    { LTk _ TokenUntil         }
  when                     { LTk _ TokenWhen          }
  while                    { LTk _ TokenWhile         }

-- Numbers, Strings, Identifiers

  stringLitToken           { LTk _ (TokenString s)    }
  binLitToken              { LTk _ (TokenBin b)       }
  maskLitToken             { LTk _ (TokenMask m)      }
  hexLitToken              { LTk _ (TokenHex h)       }
  realLitToken             { LTk _ (TokenReal r)      }
  intLitToken              { LTk _ (TokenInt i)       }

  qualifierToken           { LTk _ (TokenQualifier a) }

  identifierToken          { LTk _ (TokenIdent i)     }

-- Punctuation
  '!'                      { LTk _ TokenBang          }
  '!='                     { LTk _ TokenNeq           }
  '&&'                     { LTk _ TokenAmpAmp        }
  '&'                      { LTk _ TokenAmp           }
  '('                      { LTk _ TokenLParen        }
  ')'                      { LTk _ TokenRParen        }
  '*'                      { LTk _ TokenStar          }
  '+'                      { LTk _ TokenPlus          }
  '++'                     { LTk _ TokenPlusPlus      }
  '+:'                     { LTk _ TokenPlusColon     }
  ','                      { LTk _ TokenComma         }
  '-'                      { LTk _ TokenMinus         }
  '.'                      { LTk _ TokenDot           }
  '..'                     { LTk _ TokenDotDot        }
  '/'                      { LTk _ TokenSlash         }
  ':'                      { LTk _ TokenColon         }
  ';'                      { LTk _ TokenSemi          }
  '<'                      { LTk _ TokenLt            }
  '<<'                     { LTk _ TokenLtLt          }
  '<='                     { LTk _ TokenLtEq          }
  '='                      { LTk _ TokenEq            }
  '=='                     { LTk _ TokenEqEq          }
  '>'                      { LTk _ TokenGt            }
  '>='                     { LTk _ TokenGtEq          }
  '>>'                     { LTk _ TokenGtGt          }
  '['                      { LTk _ TokenLBrack        }
  ']'                      { LTk _ TokenRBrack        }
  '^'                      { LTk _ TokenCaret         }
  '{'                      { LTk _ TokenLBrace        }
  '||'                     { LTk _ TokenBarBar        }
  '}'                      { LTk _ TokenRBrace        }

%%

-- Macros -----------------------------------------------------------

sepRev0(s,t) :                    { [] }
             | sepRev0(s,t) s t   { $3:$1 }

sepRev1(s,t) : t                  { [$1] }
             | sepRev1(s,t) s t   { $3:$1 }

listRev0(t)  :                    { [] }
             | listRev0(t) t      { $2:$1 }

listRev1(t) : t                   { [$1] }
            | listRev1(t) t       { $2:$1 }

list0(t)  : listRev0(t)           { reverse $1 }
list1(t)  : listRev1(t)           { reverse $1 }
sep0(s,t) : sepRev0(s,t)          { reverse $1 }
sep1(s,t) : sepRev1(s,t)          { reverse $1 }
opt(t)    :                       { Nothing    }
          | t                     { Just $1    }

-- Identifiers ------------------------------------------------------

identifier :: { Identifier }
identifier : identifierToken { let (TokenIdent i) = ltToken $1 in i }

qualifier :: { ArchName }
qualifier : qualifierToken   { let (TokenQualifier a) = ltToken $1 in a }

qualId :: { QualifiedIdentifier }
qualId : identifier               { QualifiedIdentifier Nothing $1   }
                    | qualifier '.' identifier { QualifiedIdentifier (Just $1) $3 }

symDecl :: { (identifier, Type) }
symDecl : type identifier         { ($2, $1) }

-- Literals ---------------------------------------------------------

intLit :: { Integer }
intLit : intLitToken                     { let TokenInt i = ltToken $1 in i    }

hexLit :: { Integer }
hexLit : hexLitToken                     { let TokenHex i = ltToken $1 in i    }

stringLit :: { String }
stringLit : stringLitToken               { let TokenString i = ltToken $1 in i }

binLit :: { BitVector }
binLit : binLitToken                     { let TokenBin i = ltToken $1 in i }

maskLit :: { [MaskBit] }
maskLit : maskLitToken                   { let TokenMask i = ltToken $1 in i }

-- Declarations -----------------------------------------------------

definitions :: { [Definition] }
definitions : list0(definition)  { $1 }
            ;

definition :: { Definition }
definition : typeDefinition                                                { $1 }
           | varConstDefinition                                            { $1 }
           | functionDefinition                                            { $1 }
           | procedureDefinition                                           { $1 }
           | getterDefinition                                              { $1 }
           | setterDefinition                                              { $1 }

typeDefinition :: { Definition }
typeDefinition : builtin 'type' identifier ';'                             { TypeDefinition (TypeDefBuiltin $3) }
               | 'type' identifier ';'                                     { TypeDefinition (TypeDefAbstract $2) }
               | 'type' identifier '=' type ';'                            { TypeDefinition (TypeDefAlias $2 $4) }
               | 'type' identifier is '(' sep0(',', symDecl) ')'           { TypeDefinition (TypeDefStruct $2 $5) }
               | enumeration identifier '{' sep0(',', identifier) '}' ';'  { TypeDefinition (TypeDefEnum $2 $4) }

varConstDefinition :: { Definition }
varConstDefinition : type qualId ';'                                       { VariableDefinition $2 $1 }
                   | constant type identifier '=' expr ';'                 { ConstDefinition $3 $2 $5 }
                   | array type identifier '[' ixType ']' ';'              { ArrayDefinition $3 $2 $5 }

functionDefinition :: { Definition }
functionDefinition : returnType qualId '(' sep0(',', symDecl) ')' ';'    { FunctionDefinition (CallableDef $2 $4 $1 []) }
                   | returnType qualId '(' sep0(',', symDecl) ')' block  { FunctionDefinition (CallableDef $2 $4 $1 $6) }

procedureDefinition :: { Definition }
procedureDefinition : qualId '(' sep0(',', symDecl) ')' ';'              { ProcedureDefinition (CallableDef $1 $3 [] []) }
                    | qualId '(' sep0(',', symDecl) ')' block            { ProcedureDefinition (CallableDef $1 $3 [] $5) }

getterDefinition :: { Definition }
getterDefinition : returnType qualId block ';'                          { GetterDefinition $2 [] $1 $3 }
                 | returnType qualId '[' sep0(',', symDecl) ']' block   { GetterDefinition $2 $4 $1 $6 }
                 | returnType qualId '[' sep0(',', symDecl) ']' ';'     { GetterDefinition $2 $4 $1 [] }

setterDefinition :: { Definition }
setterDefinition : qualId '[' sep0(',', setterArg) ']' '=' symDecl ';'   { SetterDefinition $1 $3 $6 [] }
                 | qualId '[' sep0(',', setterArg) ']' '=' symDecl block { SetterDefinition $1 $3 $6 $7 }
                 | qualId '=' symDecl ';'                                { SetterDefinition $1 [] $3 [] }
                 | qualId '=' symDecl block                              { SetterDefinition $1 [] $3 $4 }

setterArg: type opt('&') identifier                                      { SetterArg ($3,$1) (isJust $2) }

returnType :: { [Type] }
returnType : type                                                        { [$1] }
           | '(' sep0(',', type) ')'                                     { $2   }

-- Types ------------------------------------------------------------

type :: { Type }
type : identifier                                  { TypeRef $1        }
     | identifier '(' expr ')'                     { TypeFun $1 $3     }  -- TODO: is this really a type fun?
     | typeof '(' expr ')'                         { TypeOf $3         }
     | register intLit '{' sep0(',', regField) '}' { TypeReg $2 $4     }
     | array '[' ixType ']' of type                { TypeArray $6 $3   }

regField :: { RegField }
regField : sep1(',', slice) identifier            { RegField $2 $1    }

ixType :: { IndexType }
ixType : identifier                               { IxTypeRef $1      }
       | expr '..' expr                           { IxTypeRange $1 $3 }

-- Statements -------------------------------------------------------

block :: { [Stmt] }
block : indent list0(stmt) unindent { $2 }

blockOrEmbed0 :: { [Stmt] }
blockOrEmbed0 : block          { $1 }
              | list0(stmt)    { $1 }

blockOrEmbed1 :: { [Stmt] }
blockOrEmbed1 : block          { $1 }
              | list1(stmt)    { $1 }

stmt :: { Stmt }
stmt : type sep1(',', identifier) ';'                      { StmtDecl $1 $2      }
     | symDecl '=' expr ';'                                { StmtDeclInit $1 $3  }
     | constant symDecl '=' expr ';'                       { StmtDeclConst $2 $4 }
     | lValExpr '=' expr ';'                               { StmtAssign $1 $3    }
     | qualId '(' sep0(',', expr) ')' ';'                  { StmtCall $1 $3 }
     | return opt(expr) ';'                                { StmtReturn $2 }
     | assert expr ';'                                     { StmtAssert $2 }
     | UNPREDICTABLE ';'                                   { StmtUnpredictable }
     | IMPLEMENTATION_DEFINED stringLit ';'                { StmtImpDef $2 }
     | if expr then blockOrEmbed1 list0(elsIf)                    { StmtIfThen (($2,$4):$5) Nothing }
     | if expr then blockOrEmbed1 list0(elsIf) else blockOrEmbed1 { StmtIfThen (($2,$4):$5) (Just $7)}
     | case expr of indent list1(caseAlt) unindent         { StmtCase $2 $5 }
     | for identifier '=' forBounds block                  { StmtFor $2 $4 $5 }
     | while expr do block                                 { StmtWhile $2 $4 }
     | repeat block until expr ';'                         { StmtRepeat $2 $4 }
     | throw identifier ';'                                { StmtThrow $2 }
     | UNDEFINED ';'                                       { StmtUndefined }
     | SEE '(' expr ')' ';'                                { StmtSeeExpr $3 }
     | SEE stringLit ';'                                   { StmtSeeString $2 }
     | try block catch identifier indent list1(catchAlt) unindent { StmtTry $2 $4 $6 }

catchAlt :: { CatchAlternative }
catchAlt : when expr block                                 { CatchWhen $2 $3   }
         | otherwise block                                 { CatchOtherwise $2 }

forBounds :: { (Expr, Expr) }
forBounds : expr to expr                                   { ($1, $3) }
          | expr downto expr                               { ($3, $1) }

elsIf :: { (Expr,[Stmt]) }
elsIf : elsif expr then blockOrEmbed1                      { ($2, $4) }

caseAlt :: { CaseAlternative }
caseAlt : when sep0(',', casePattern) opt(caseGuard) blockOrEmbed0  { CaseWhen $2 $3 $4 }
        | otherwise blockOrEmbed0                                   { CaseOtherwise $2 }


casePattern :: { CasePattern }
casePattern : intLit                                       { CasePatternInt $1 }
            | hexLit                                       { CasePatternHex $1 }
            | binLit                                       { CasePatternBin $1 }
            | maskLit                                      { CasePatternMask $1 }
            | identifier                                   { CasePatternIdentifier $1 }
            | '-'                                          { CasePatternIgnore }
            | '(' sep1(',', casePattern) ')'               { CasePatternSeq $2 }

caseGuard :: { Expr }
caseGuard :  '&&' expr                                     { $2 }

lValExpr :: { LValExpr }
lValExpr : '-'                                             { LValDiscard }
         | qualId                                          { LValVarRef $1 }
         | lValExpr '.' identifier                         { LValMember $1 $3 }
         | lValExpr '.' '[' sep1(',', identifier) ']'      { LValDotIndex $1 $4 }
         | lValExpr '[' sep1(',', slice) ']'               { LValIndex $1 $3 }
         | '[' sep1(',', lValExpr) ']'                     { LValArrayPattern $2 }
         | '(' sep1(',', lValExpr) ')'                     { LValList $2 }

-- TODO:

slice : { Slice }
slice : expr                           { Slice }

expr :: { Expr }
expr : identifier                      { Expr }

{

happyError :: [LocatedToken] -> a
happyError ((LTk (l,c) t):_) = error $ show l ++ ":" ++ show c ++ " Syntax error at " ++ show t
happyError [] = error $ "Syntax error at EOF"

}