grammar ASL;

tokens { INDENT, DEDENT }


// -- DEFINITIONS ---------------------------------------------------

definitions : definition* EOF ;

definition:
      '__builtin' 'type' IDENTIFIER ';'                                  #DefTypeBuiltin
    | 'type' IDENTIFIER ';'                                              #DefTypeAbstract
    | 'type' IDENTIFIER '=' type ';'                                     #DefTypeAlias
    | 'type' qualId     'is' '(' symDeclCommaList ')'                    #DefTypeStruct
    | 'enumeration' IDENTIFIER '{' identifierCommaList0 '}' ';'          #DefTypeEnum
    | type qualId ';'                                                    #DefVariable
    | 'constant' type IDENTIFIER '=' expr ';'                            #DefConstant
    | 'array' type IDENTIFIER '[' ixType ']' ';'                         #DefArray
    | returnType? qualId '(' symDeclCommaList ')' (indentedBlock | ';')  #DefCallable
    | returnType qualId indentedBlock                                    #DefGetter
    | returnType qualId '[' symDeclCommaList ']'  (indentedBlock | ';')  #DefGetter
    | qualId ('[' (setterArg (',' setterArg)*)? ']')? '=' symDecl
             (indentedBlock | ';')                                       #DefSetter
    ;


setterArg:
      type '&' IDENTIFIER                                                #SetterRefArg
    | type IDENTIFIER                                                    #SetterValArg
    ;

// -- TYPES ---------------------------------------------------------

returnType:
      type
    | '(' type (',' type)* ')'
    ;

type:
      //IDENTIFIER
      qualId                                              #TypeRef   // is this really the case sometimes?  see out.asl:9657
    | IDENTIFIER '(' expr ')'                             #TypeIndexed
    | 'typeof' '(' expr ')'                               #TypeOf
    | 'register' NAT_LIT '{' regField (',' regField)* '}' #TypeRegister
    | 'array' '[' ixType ']' 'of' type                    #TypeArray
    ;

ixType:
      IDENTIFIER                                          #IxTypeIdentifier
    | begin=expr '..' end=expr                            #IxTypeRange
    ;

regField: slice (',' slice)* IDENTIFIER ;


// -- STATEMENTS ----------------------------------------------------

indentedBlock: INDENT stmt* DEDENT ;

blockOrEmbed0:
      INDENT stmt* DEDENT
    | stmt*
    ;

blockOrEmbed1:
      INDENT stmt* DEDENT
    | stmt+
    ;

stmt:
      type identifierCommaList0 ';'                       #StmtVarsDecl
    | symDecl '=' expr ';'                                #StmtVarDeclInit
    | 'constant' symDecl '=' expr ';'                     #StmtConstDecl
    | lValExpr '=' expr ';'                               #StmtAssign
    | qualId '(' exprCommaList0 ')' ';'                   #StmtCall
    | 'return' expr? ';'                                  #StmtReturn
    | 'assert' expr ';'                                   #StmtAssert
    | 'UNPREDICTABLE' ';'                                 #StmtUnpredictable
    | 'IMPLEMENTATION_DEFINED' STRING_LIT ';'             #StmtImpDef
    | 'if' test=expr 'then' thenExpr=blockOrEmbed1
      (stmtElsIf)*
      ('else' elseExpr=blockOrEmbed1)?                    #StmtIf
    | 'case' expr 'of' INDENT caseAlt+ DEDENT             #StmtCase
    | 'for' IDENTIFIER '='
        begin=expr direction=('to'|'downto') end=expr
        indentedBlock                                     #StmtFor
    | 'while' expr 'do' indentedBlock                     #StmtWhile
    | 'repeat' indentedBlock 'until' expr ';'             #StmtRepeat
    | 'throw' IDENTIFIER ';'                              #StmtThrow
    | 'UNDEFINED' ';'                                     #StmtUndefined
    | 'SEE' '(' expr ')' ';'                              #StmtSeeExpr
    | 'SEE' STRING_LIT ';'                                #StmtSeeStrLit
    | 'try' indentedBlock
      'catch' IDENTIFIER INDENT catchAlt+ DEDENT          #StmtTry
    ;

stmtElsIf:'elsif' expr 'then' blockOrEmbed1 ;

catchAlt:
      'when' expr indentedBlock                           #CatchAltWhen
    | 'otherwise' indentedBlock                           #CatchAltOtherwise
    ;

caseAlt:
      'when' casePattern (',' casePattern)*
        ('&&' expr)? blockOrEmbed0                        #CaseAltWhen
    | 'otherwise' blockOrEmbed0                           #CaseAltOtherwise
    ;

casePattern:
      NAT_LIT                                             #CasePatternNat
    | HEX_LIT                                             #CasePatternHex
    | BIN_LIT                                             #CasePatternBin
    | MASK_LIT                                            #CasePatternMask
    | IDENTIFIER                                          #CasePatternBind
    | '-'                                                 #CasePatternIgnore
    | '(' casePattern (',' casePattern)* ')'              #CasePatternTuple
    ;


lValExpr:
      '-'                                                 #LValIgnore
    | lValExpr '.' IDENTIFIER                             #LValMember
    | lValExpr '.' '[' identifierCommaList1 ']'           #LValMemberArray
    | lValExpr '[' (slice (',' slice)*)? ']'              #LValArrayIndex
    | '[' lValExpr (',' lValExpr)* ']'                    #LValArray
    | '(' lValExpr (',' lValExpr)* ')'                    #LValTuple
    | lValExpr '<' sliceCommaList1 '>'                    #LValSliceOf
    | lValExpr '.' '<' identifierCommaList1 '>'           #LValMemberBits
    | '<' lValExpr (',' lValExpr)* '>'                    #LValSlice
    | qualId                                              #LValVarRef
    ;


// -- EXPRESSIONS ---------------------------------------------------


expr:
      NAT_LIT                                             #ExprLitNat
    | HEX_LIT                                             #ExprLitHex
    | REAL_LIT                                            #ExprLitReal
    | BIN_LIT                                             #ExprLitBin
    | MASK_LIT                                            #ExprLitMask
    | STRING_LIT                                          #ExprLitString
    | qualId                                              #ExprVarRef
    | qualId '(' exprCommaList0 ')'                       #ExprCall
    | '(' exprCommaList1 ')'                              #ExprTuple
    | operator=('-' | '!') expr                           #ExprUnOp
    | type 'UNKNOWN'                                      #ExprUnknown
    | type 'IMPLEMENTATION_DEFINED' STRING_LIT?           #ExprImpDef
    | expr '.' IDENTIFIER                                 #ExprMember
    | expr '.' '[' identifierCommaList1 ']'               #ExprMembers
    | expr '[' sliceCommaList0 ']'                        #ExprIndex
    | expr 'IN' set                                       #ExprInSet
    | expr 'IN' MASK_LIT                                  #ExprInMask
    | operand1=expr
      operator=( '==' | '!=' | '>'  | '>=' | '>>' | '<'  | '<=' | '<<'
                 | '+'  | '-'  | '*'  | '/'  | '^'  | '&&' | '||' | 'OR'
                 | 'EOR' | 'AND' | '++' | 'QUOT' | 'REM' | 'DIV' | 'MOD')
      operand2=expr                                       #ExprBinOp
    | operand1=expr ':' operand2=expr                     #ExprConcat
    | expr '<' sliceCommaList1 '>'                        #ExprSlice
    | expr '.' '<' identifierCommaList1 '>'               #ExprMemberBits //?
    | 'if' test=expr 'then' thenExpr=expr
      exprElsIf*
      'else' elseExpr=expr                                #ExprIf

    ;

exprElsIf: 'elsif' test=expr 'then' result=expr ;

slice:
      base=expr '+:' count=expr                           #SliceOffset
    | expr                                                #SliceSingle
    ;

setElement: begin=expr '..' end=expr                      #SetElementRange
          | expr                                          #SetElementSingle
          ;

set: '{' (setElement (',' setElement)*)? '}' ;

sliceCommaList0: (slice (',' slice)*)? ;
sliceCommaList1: (slice (',' slice)*) ;

exprCommaList1: expr (',' expr)* ;
exprCommaList0: (expr (',' expr)*)? ;

// -- BASIC STUFF ---------------------------------------------------

symDecl: type IDENTIFIER ;
identifierCommaList0: (IDENTIFIER (',' IDENTIFIER)*)? ;
identifierCommaList1: IDENTIFIER (',' IDENTIFIER)* ;
symDeclCommaList: (symDecl (',' symDecl)*)? ;

qualId:
      IDENTIFIER                         #QualIdUnqualified
    | 'AArch32' '.' IDENTIFIER           #QualIdAArch32
    | 'AArch64' '.' IDENTIFIER           #QualIdAArch64
    ;

IDENTIFIER : [A-Za-z_][A-Za-z0-9_]* ;
NAT_LIT    : [0-9]+ ;
HEX_LIT    : '0x' [0-9a-fA-F]+ ;
BIN_LIT    : '\'' [01 ]* '\'' ;
MASK_LIT   : '\'' [01x ]* '\'' ;
REAL_LIT   : [0-9]+ '.' [0-9]+ ;
STRING_LIT : '"' ~'"'* '"' ;

COMMENT : '/*' (COMMENT|.)*? '*/' -> skip ;
LINE_COMMENT : '//' .*? '\n'      -> skip ;
WS : [ \n\u000D\t]                -> skip ;
