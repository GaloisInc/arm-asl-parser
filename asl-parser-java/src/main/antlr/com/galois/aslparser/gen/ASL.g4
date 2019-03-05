grammar ASL;

tokens { INDENT, DEDENT }

// -- INSTRUCTIONS --------------------------------------------------

instructions: instruction* EOF ;

instruction: '__instruction' id INDENT encoding+ '__execute' indentedBlock DEDENT ;

encoding:
    '__encoding' id
    INDENT
        '__instruction_set' instructionSet=('A32'|'T32'|'T16')
        instructionField*
        '__opcode' opcode=(MASK_LIT|BIN_LIT)
        '__guard' expr
        instrUnpredictableUnless*
        '__decode' (decode=indentedBlock)?
    DEDENT;

instructionField: '__field' id begin=NAT_LIT '+:' len=NAT_LIT ;
instrUnpredictableUnless: '__unpredictable_unless' idx=NAT_LIT '==' bin=BIN_LIT ;

// -- DEFINITIONS ---------------------------------------------------

definitions : definition* EOF ;

definition:
      '__builtin' 'type' id ';'                                          #DefTypeBuiltin
    | 'type' id ';'                                                      #DefTypeAbstract
    | 'type' id '=' type ';'                                             #DefTypeAlias
    | 'type' qualId     'is' '(' symDeclCommaList ')'                    #DefTypeStruct
    | 'enumeration' id '{' identifierCommaList0 '}' ';'                  #DefTypeEnum
    | type qualId ';'                                                    #DefVariable
    | 'constant' type id '=' expr ';'                                    #DefConstant
    | 'array' type id '[' ixType ']' ';'                                 #DefArray
    | returnType? qualId '(' symDeclCommaList ')' (indentedBlock | ';')  #DefCallable
    | returnType qualId indentedBlock                                    #DefGetter
    | returnType qualId '[' symDeclCommaList ']'  (indentedBlock | ';')  #DefGetter
    | qualId ('[' (setterArg (',' setterArg)*)? ']')? '=' symDecl
             (indentedBlock | ';')                                       #DefSetter
    ;


setterArg:
      type '&' id                                                #SetterRefArg
    | type id                                                    #SetterValArg
    ;

// -- TYPES ---------------------------------------------------------

returnType:
      type
    | '(' type (',' type)* ')'
    ;

type:
      //IDENTIFIER
      qualId                                              #TypeRef   // is this really the case sometimes?  see out.asl:9657
    | id '(' expr ')'                                     #TypeIndexed
    | 'typeof' '(' expr ')'                               #TypeOf
    | 'register' NAT_LIT '{' regField (',' regField)* '}' #TypeRegister
    | 'array' '[' ixType ']' 'of' type                    #TypeArray
    ;

ixType:
      id                                                  #IxTypeIdentifier
    | begin=expr '..' end=expr                            #IxTypeRange
    ;

regField: slice (',' slice)* id ;


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
    | 'for' id '='
        begin=expr direction=('to'|'downto') end=expr
        indentedBlock                                     #StmtFor
    | 'while' expr 'do' indentedBlock                     #StmtWhile
    | 'repeat' indentedBlock 'until' expr ';'             #StmtRepeat
    | 'throw' id ';'                                      #StmtThrow
    | 'UNDEFINED' ';'                                     #StmtUndefined
    | SEE_TOK ';'                                         #StmtSee
    | 'try' indentedBlock
      'catch' id INDENT catchAlt+ DEDENT                  #StmtTry
    | 'enumeration' id '{' identifierCommaList0 '}' ';'   #StmtDefEnum
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
    | id                                          #CasePatternBind
    | '-'                                                 #CasePatternIgnore
    | '(' casePattern (',' casePattern)* ')'              #CasePatternTuple
    ;


lValExpr:
      '-'                                                 #LValIgnore
    | lValExpr '.' id                             #LValMember
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
    | expr '.' id                                 #ExprMember
    | expr '.' '[' identifierCommaList1 ']'               #ExprMembers
    | expr '[' sliceCommaList0 ']'                        #ExprIndex
    | expr 'IN' set                                       #ExprInSet
    | expr 'IN' MASK_LIT                                  #ExprInMask
    | operand1=expr operator='^' operand1=expr            #ExprBinOp
    | operand1=expr operator=('*' | '/') operand2=expr    #ExprBinOp
    | operand1=expr operator=('+' | '-') operand2=expr    #ExprBinOp
    | operand1=expr operator=('>>' | '<<' |  'QUOT' | 'REM' | 'DIV' | 'MOD' | 'OR' | 'EOR' | 'AND' | '++' | ':') operand2=expr  #ExprBinOp
    | operand1=expr operator=('==' | '!=' | '>' | '>=' | '<'  | '<=') operand2=expr                #ExprBinOp
    | operand1=expr operator=( '&&' | '||' )  operand2=expr   #ExprBinOp
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

symDecl: type id ;
identifierCommaList0: (id (',' id)*)? ;
identifierCommaList1: id (',' id)* ;
symDeclCommaList: (symDecl (',' symDecl)*)? ;

qualId:
      id                         #QualIdUnqualified
    | 'AArch32' '.' id           #QualIdAArch32
    | 'AArch64' '.' id           #QualIdAArch64
    ;

id: IDENTIFIER | 'register' | 'enumeration';

INDENT: 'IND' ;
DEDENT: 'DED' ;

SEE_TOK : 'SEE' ~';'+ ;
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
