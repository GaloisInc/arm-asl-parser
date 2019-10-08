grammar ASL;

tokens { INDENT, DEDENT }

// -- REGISTERS ----------------------------------------------

registers: registerDefinition* EOF ;

registerDefinition:
      arrayRegister  #RegDefArray
    | register       #RegDefBasic
    ;

register: '__register' NAT_LIT '{' registerFieldCommaList '}' id SEMICOLON ;

arrayRegister: 'array' '[' lo=NAT_LIT '..' hi=NAT_LIT ']' 'of' register ;

registerField: hi=NAT_LIT ':' lo=NAT_LIT id? ;

registerFieldCommaList: (registerField (',' registerField)*)? ;

// -- INSTRUCTIONS --------------------------------------------------

instructions: instruction* EOF ;

instruction: '__instruction' idWithDots
    INDENT encoding+
           ('__postdecode' postDecodeBlock=indentedBlock?)?
           '__execute' conditional='__conditional'? executeBlock=indentedBlock?
    DEDENT ;

encoding:
    '__encoding' idWithDots
    INDENT
        '__instruction_set' instructionSet=('A64'|'A32'|'T32'|'T16')
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
      '__builtin' 'type' id SEMICOLON                                    #DefTypeBuiltin
    | 'type' id SEMICOLON                                                #DefTypeAbstract
    | 'type' id '=' type SEMICOLON                                       #DefTypeAlias
    | 'type' qualId     'is' '(' symDeclCommaList ')'                    #DefTypeStruct
    | 'enumeration' id '{' identifierCommaList0 '}' SEMICOLON            #DefTypeEnum
    | type qualId SEMICOLON                                              #DefVariable
    | 'constant' type id '=' expr SEMICOLON                              #DefConstant
    | 'array' type id '[' ixType ']' SEMICOLON                           #DefArray
    | returnType? qualId '(' symDeclCommaList ')'
        (indentedBlock | SEMICOLON)                                      #DefCallable
    | returnType qualId indentedBlock                                    #DefGetter
    | returnType qualId '[' symDeclCommaList ']'
        (indentedBlock | SEMICOLON)                                      #DefGetter
    | qualId '[' setterArgCommaList ']' '=' symDecl
        (indentedBlock | SEMICOLON)                                      #DefSetter
    | qualId '=' symDecl (indentedBlock | SEMICOLON)                     #DefSetter            
    ;


setterArg:
      type '&' id                                                        #SetterRefArg
    | type id                                                            #SetterValArg
    ;

setterArgCommaList: (setterArg (',' setterArg)*)? ;

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
      id                                                  #IxTypeRef
    | begin=expr '..' end=expr                            #IxTypeRange
    ;

regField: slice (',' slice)* id ;


// -- STATEMENTS ----------------------------------------------------

indentedBlock: INDENT stmt* DEDENT ;

blockOrEmbed0: blockOrEmbed1? ;

blockOrEmbed1:
      (inlineStmt ';')* stmt                              #BlockInline
    | indentedBlock                                       #BlockIndent
    ;

SEMICOLON : ';' ENDLINE ;

stmt:
      inlineStmt (SEMICOLON | ';')                        #StmtsInline
    | 'if' test=expr 'then' thenExpr=blockOrEmbed1
      (stmtElsIf)*
      ('else' elseExpr=blockOrEmbed1)?                    #StmtIf
    | 'case' expr 'of' INDENT caseAlt+ DEDENT             #StmtCase
    | 'for' id '='
        begin=expr direction=('to'|'downto') end=expr
        indentedBlock                                     #StmtFor
    | 'while' expr 'do' indentedBlock                     #StmtWhile
    | 'try' indentedBlock
      'catch' id INDENT catchAlt+ DEDENT                  #StmtTry    
    ;

inlineStmt:
      type identifierCommaList1                           #StmtVarsDecl
    | symDecl '=' expr                                    #StmtVarDeclInit
    | 'constant' symDecl '=' expr                         #StmtConstDecl
    | lValExpr '=' expr                                   #StmtAssign
    | qualId '(' exprCommaList0 ')'                       #StmtCall
    | 'return' expr?                                      #StmtReturn
    | 'assert' expr                                       #StmtAssert
    | 'UNPREDICTABLE'                                     #StmtUnpredictable
    | 'IMPLEMENTATION_DEFINED' STRING_LIT                 #StmtImpDef
    | 'repeat' indentedBlock 'until' expr                 #StmtRepeat
    | 'throw' id                                          #StmtThrow
    | 'UNDEFINED'                                         #StmtUndefined
    | SEE_TOK                                             #StmtSee
    | 'enumeration' id '{' identifierCommaList0 '}'       #StmtDefEnum
    ;

stmtElsIf:'elsif' expr 'then' blockOrEmbed1 ;

catchAlt:
      'when' expr blockOrEmbed1                           #CatchAltWhen
    | 'otherwise' blockOrEmbed1                           #CatchAltOtherwise
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
    | id                                                  #CasePatternBind
    | '-'                                                 #CasePatternIgnore
    | '(' casePattern (',' casePattern)* ')'              #CasePatternTuple
    ;


lValExpr:
      lValExpr '.' id                                     #LValMember
    | lValExpr '.' '[' identifierCommaList1 ']'           #LValMemberArray
    | lValExpr '[' (slice (',' slice)*)? ']'              #LValArrayIndex
    | '[' lValExpr (',' lValExpr)* ']'                    #LValArray
    | '(' lValExpr (',' lValExpr)* ')'                    #LValTuple
    | lValExpr '<' sliceCommaList1 '>'                    #LValSliceOf
    | lValExpr '.' '<' identifierCommaList1 '>'           #LValMemberBits
    | '<' lValExpr (',' lValExpr)* '>'                    #LValSlice
    | qualId                                              #LValVarRef
    |  '-'                                                #LValIgnore
    ;

// -- EXPRESSIONS ---------------------------------------------------

expr:
      NAT_LIT                                             #ExprLitNat
    | HEX_LIT                                             #ExprLitHex
    | REAL_LIT                                            #ExprLitReal
    | BIN_LIT                                             #ExprLitBin
    | MASK_LIT                                            #ExprLitMask
    | STRING_LIT                                          #ExprLitString
    | qualId '(' exprCommaList0 ')'                       #ExprCall
    | qualId                                              #ExprVarRef
    | '(' expr ')'                                        #ExprParen
    | '(' exprCommaList1 ')'                              #ExprTuple
    | expr '.' id                                         #ExprMember
    | operator=('-' | '!' | 'NOT') expr                   #ExprUnOp
    | type 'UNKNOWN'                                      #ExprUnknown
    | type 'IMPLEMENTATION_DEFINED' STRING_LIT?           #ExprImpDef
    | expr '.' '[' identifierCommaList1 ']'               #ExprMembers
    | expr '.' '<' identifierCommaList1 '>'               #ExprMemberBits
    | expr '[' sliceCommaList0 ']'                        #ExprIndex
    | expr 'IN' set                                       #ExprInSet
    | expr 'IN' MASK_LIT                                  #ExprInMask
    | expr '<' sliceCommaList1 '>'                        #ExprSlice
    | operand1=expr operator='^' operand2=expr            #ExprBinOp
    | operand1=expr operator=('*' | '/') operand2=expr    #ExprBinOp
    | operand1=expr operator=('-' | '+') operand2=expr    #ExprBinOp
    | operand1=expr operator=('>>' | '<<' |  'QUOT' | 'REM' | 'DIV' | 'MOD' | 'OR' | 'EOR' | 'AND' | '++' | ':') operand2=expr  #ExprBinOp
    | operand1=expr operator=('==' | '!=' | '>' | '>=' | '<'  | '<=') operand2=expr #ExprBinOp
    | operand1=expr operator=( '&&' | '||' )  operand2=expr   #ExprBinOp
    | 'if' test=expr 'then' thenExpr=expr
      exprElsIf*
      'else' elseExpr=expr                                #ExprIf

    ;

// this is a kind of a hack to keep SliceRange from being parsed as a concat
// which is necessary to preserve precedence rules - in this case we
// have a version of expr that could in principle return an integer
sliceExpr:
      NAT_LIT                                             #SliceExprLitNat
    | HEX_LIT                                             #SliceExprLitHex
    | '(' sliceExpr ')'                                   #SliceExprParen
    | qualId                                              #SliceExprVarRef
    | qualId '(' exprCommaList0 ')'                       #SliceExprCall
    | operator=('-' | '!') operand=sliceExpr              #SliceExprUnOp
    | expr '.' id                                         #SliceExprMember
    | operand1=sliceExpr operator='^' operand2=sliceExpr            #SliceExprBinOp
    | operand1=sliceExpr operator=('*' | '/') operand2=sliceExpr    #SliceExprBinOp
    | operand1=sliceExpr operator=('-' | '+') operand2=sliceExpr    #SliceExprBinOp

    | operand1=sliceExpr operator=('>>' | '<<' |  'QUOT' | 'REM' | 'DIV' | 'MOD' | 'OR' | 'EOR' | 'AND' | '++') operand2=sliceExpr  #SliceExprBinOp
        | 'if' test=expr 'then' thenExpr=expr
          exprElsIf*
          'else' elseExpr=expr                            #SliceExprIf
    ;

slice:
      begin=sliceExpr ':' end=sliceExpr                   #SliceRange
    | base=sliceExpr '+:' count=sliceExpr                 #SliceOffset
    | expr                                                #SliceSingle
    ;


exprElsIf: 'elsif' test=expr 'then' result=expr ;


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

idWithDots: id ('.' (id|NAT_LIT))* ;

id: IDENTIFIER | 'register' | 'enumeration' | 'NOT';

//INDENT: 'IND' ;
//DEDENT: 'DED' ;


IDENTIFIER : [A-Za-z_][A-Za-z0-9_]* ;
NAT_LIT    : [0-9]+ ;
HEX_LIT    : '0x' [0-9a-fA-F]+ ;
BIN_LIT    : '\'' [01 ]* '\'' ;
MASK_LIT   : '\'' [01x ]* '\'' ;
REAL_LIT   : [0-9]+ '.' [0-9]+ ;
STRING_LIT : '"' ~'"'* '"' ;

SEE_TOK : 'SEE ' ~';'+ ;

COMMENT : '/*' (COMMENT|.)*? '*/' -> skip ;
LINE_COMMENT : ' '* '//' .*? '\n'      -> skip ;
ENDLINE : ' '* ('//' .*?)? '\n'          -> skip ;

WS : [ \n\u000D\t]                -> skip ;
