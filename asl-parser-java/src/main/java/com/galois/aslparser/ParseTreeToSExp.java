package com.galois.aslparser;


import com.galois.aslparser.gen.ASLBaseVisitor;
import com.galois.aslparser.gen.ASLParser;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static com.galois.aslparser.SExp.*;

public class ParseTreeToSExp extends ASLBaseVisitor<SExp> {
    private SExp sub(ParseTree tree) {
        return this.visit(tree);
    }

    private List<SExp> subs(List<? extends ParseTree> cs) {
        return cs.stream().map(this::sub).collect(Collectors.toList());
    }

    private SExp id(ParseTree identifier) {
        return atom(identifier.getText());
    }

    private SExp list(List<SExp> pt) {
        return sexp("list", pt);
    }

    private SExp nat(TerminalNode term) {
        return atom(term.getText());
    }

    private SExp nat(Token begin) {
        return atom(begin.getText());
    }

    private SExp maybe(ParseTree p) {
        if(p == null)
            return atom("Nothing");
        else
            return sexp("Just", sub(p));
    }

    private SExp stringLit(TerminalNode string_lit) {
        return atom(string_lit.getText());
    }

    private SExp hex(TerminalNode hex_lit) {
        return atomq(hex_lit.getText());
    }

    private SExp bin(TerminalNode bin_lit) {
        return atom(bin_lit.getText());
    }

    private SExp mask(TerminalNode mask_lit) {
        return atom(mask_lit.getText());
    }

    private SExp real(TerminalNode real_lit) {
        return atom(real_lit.getText());
    }

    @Override
    public SExp visitIndentedBlock(ASLParser.IndentedBlockContext ctx) {
        return sexp("StmtBlock", list(subs(ctx.stmt())));
    }

    @Override
    public SExp visitBlockOrEmbed0(ASLParser.BlockOrEmbed0Context ctx) {
        return sexp("StmtBlock", list(subs(ctx.stmt())));
    }

    @Override
    public SExp visitExprParen(ASLParser.ExprParenContext ctx) {
        return sub(ctx.expr());
    }

    @Override
    public SExp visitBlockOrEmbed1(ASLParser.BlockOrEmbed1Context ctx) {
        return sexp("StmtBlock", list(subs(ctx.stmt())));
    }

    @Override
    public SExp visitIfEmbed(ASLParser.IfEmbedContext ctx) {
        return sexp("StmtBlock", list(subs(ctx.stmt())));
    }

    @Override
    public SExp visitReturnType(ASLParser.ReturnTypeContext ctx) {
        return sexp("ReturnType", list(subs(ctx.type())));
    }

    @Override
    public SExp visitSymDeclCommaList(ASLParser.SymDeclCommaListContext ctx) {
        return list(subs(ctx.symDecl()));
    }

    @Override
    public SExp visitIdentifierCommaList0(ASLParser.IdentifierCommaList0Context ctx) {
        return list(ctx.id().stream().map(this::id).collect(Collectors.toList()));
    }

    @Override
    public SExp visitIdentifierCommaList1(ASLParser.IdentifierCommaList1Context ctx) {
        return list(ctx.id().stream().map(this::id).collect(Collectors.toList()));
    }

    @Override
    public SExp visitSymDecl(ASLParser.SymDeclContext ctx) {
        return sexp("SymDecl", id(ctx.id()), sub(ctx.type()));
    }

    @Override
    public SExp visitQualIdUnqualified(ASLParser.QualIdUnqualifiedContext ctx) {
        return sexp("QualifiedIdentifier", atom("Any"), id(ctx.id()));
    }

    @Override
    public SExp visitQualIdAArch32(ASLParser.QualIdAArch32Context ctx) {
        return sexp("QualifiedIdentifier", atom("AArch32"), id(ctx.id()));
    }

    @Override
    public SExp visitQualIdAArch64(ASLParser.QualIdAArch64Context ctx) {
        return sexp("QualifiedIdentifier", atom("AArch64"), id(ctx.id()));
    }

    @Override
    public SExp visitDefinitions(ASLParser.DefinitionsContext ctx) {
        return sexp("AslDefinitions", subs(ctx.definition()));
    }

// -- INSTRUCTIONS --------------------------------------------------

    @Override
    public SExp visitInstruction(ASLParser.InstructionContext ctx) {
        return sexp("Instruction",
                    id(ctx.idWithDots()),
                    list(subs(ctx.encoding())),
                    maybe(ctx.postDecodeBlock),
                    maybe(ctx.executeBlock));
    }

    @Override
    public SExp visitEncoding(ASLParser.EncodingContext ctx) {
        return sexp("InstructionEncoding",
                    id(ctx.idWithDots()),
                    atom(ctx.instructionSet.getText()),
                    list(subs(ctx.instructionField())),
                    atom(ctx.opcode.getText()),
                    maybe(ctx.expr()),
                    list(subs(ctx.instrUnpredictableUnless())),
                    maybe(ctx.decode));
    }

    private SExp emptyList() {
        return list(Collections.emptyList());
    }

    @Override
    public SExp visitInstructionField(ASLParser.InstructionFieldContext ctx) {
        return sexp("InstructionField", id(ctx.id()), nat(ctx.begin), nat(ctx.len));
    }

    @Override
    public SExp visitInstrUnpredictableUnless(ASLParser.InstrUnpredictableUnlessContext ctx) {
        return sexp("InstructionUnpredictableUnless", nat(ctx.idx), bin(ctx.BIN_LIT()));
    }

// -- DEFINITIONS ---------------------------------------------------

    @Override
    public SExp visitDefTypeBuiltin(ASLParser.DefTypeBuiltinContext ctx) {
        return sexp("DefTypeBuiltin", id(ctx.id()));
    }

    @Override
    public SExp visitDefTypeAbstract(ASLParser.DefTypeAbstractContext ctx) {
        return sexp("DefTypeAbstract", id(ctx.id()));
    }

    @Override
    public SExp visitDefTypeAlias(ASLParser.DefTypeAliasContext ctx) {
        return sexp("DefTypeAlias", id(ctx.id()), sub(ctx.type()));
    }

    @Override
    public SExp visitDefTypeStruct(ASLParser.DefTypeStructContext ctx) {
        return sexp("DefTypeStruct", sub(ctx.qualId()), sub(ctx.symDeclCommaList()));
    }

    @Override
    public SExp visitDefTypeEnum(ASLParser.DefTypeEnumContext ctx) {
        return sexp("DefTypeEnum", id(ctx.id()), sub(ctx.identifierCommaList0()));
    }

    @Override
    public SExp visitDefVariable(ASLParser.DefVariableContext ctx) {
        return sexp("DefVariable", sub(ctx.qualId()), sub(ctx.type()));
    }

    @Override
    public SExp visitDefConstant(ASLParser.DefConstantContext ctx) {
        return sexp("DefConst", id(ctx.id()), sub(ctx.type()), sub(ctx.expr()));
    }

    @Override
    public SExp visitDefArray(ASLParser.DefArrayContext ctx) {
        return sexp("DefArray", id(ctx.id()), sub(ctx.type()), sub(ctx.ixType()));
    }

    @Override
    public SExp visitDefCallable(ASLParser.DefCallableContext ctx) {
        return sexp("DefCallable",
                sub(ctx.qualId()),
                sub(ctx.symDeclCommaList()),
                maybe(ctx.returnType()),
                maybe(ctx.indentedBlock()));
    }

    @Override
    public SExp visitDefGetter(ASLParser.DefGetterContext ctx) {
        return sexp("DefGetter",
                sub(ctx.qualId()),
                maybe(ctx.symDeclCommaList()),
                sub(ctx.returnType()),
                maybe(ctx.indentedBlock()));
    }

    @Override
    public SExp visitDefSetter(ASLParser.DefSetterContext ctx) {
        return sexp("DefSetter",
                sub(ctx.qualId()),
                list(subs(ctx.setterArg())),
                sub(ctx.symDecl()),
                maybe(ctx.indentedBlock()));
    }

    @Override
    public SExp visitSetterRefArg(ASLParser.SetterRefArgContext ctx) {
        return sexp("SetterArg", id(ctx.id()), sub(ctx.type()), atom("Reference"));
    }

    @Override
    public SExp visitSetterValArg(ASLParser.SetterValArgContext ctx) {
        return sexp("SetterArg", id(ctx.id()), sub(ctx.type()), atom("Value"));
    }

    @Override
    public SExp visitTypeRef(ASLParser.TypeRefContext ctx) {
        return sexp("TypeRef", sub(ctx.qualId()) );
    }

    @Override
    public SExp visitTypeIndexed(ASLParser.TypeIndexedContext ctx) {
        return sexp("TypeFun", id(ctx.id()), sub(ctx.expr()));
    }

    @Override
    public SExp visitTypeOf(ASLParser.TypeOfContext ctx) {
        return sexp("TypeOf", sub(ctx.expr()));
    }

    @Override
    public SExp visitTypeRegister(ASLParser.TypeRegisterContext ctx) {
        return sexp("TypeReg", nat(ctx.NAT_LIT()), list(subs(ctx.regField())));
    }

    @Override
    public SExp visitTypeArray(ASLParser.TypeArrayContext ctx) {
        return sexp("TypeArray", sub(ctx.type()), sub(ctx.ixType()));
    }

    @Override
    public SExp visitIxTypeRef(ASLParser.IxTypeRefContext ctx) {
        return sexp("IxTypeRef", id(ctx.id()));
    }

    @Override
    public SExp visitIxTypeRange(ASLParser.IxTypeRangeContext ctx) {
        return sexp("IxTypeRange", sub(ctx.begin), sub(ctx.end));
    }

    @Override
    public SExp visitRegField(ASLParser.RegFieldContext ctx) {
        return sexp("RegField", id(ctx.id()), list(subs(ctx.slice())));
    }

    @Override
    public SExp visitStmtVarsDecl(ASLParser.StmtVarsDeclContext ctx) {
        return sexp("StmtVarsDecl", sub(ctx.identifierCommaList1()), sub(ctx.type()));
    }

    @Override
    public SExp visitStmtVarDeclInit(ASLParser.StmtVarDeclInitContext ctx) {
        return sexp("StmtVarDeclInit", sub(ctx.symDecl()), sub(ctx.expr()));
    }

    @Override
    public SExp visitStmtConstDecl(ASLParser.StmtConstDeclContext ctx) {
        return sexp("StmtConstDecl", sub(ctx.symDecl()), sub(ctx.expr()));
    }

    @Override
    public SExp visitStmtAssign(ASLParser.StmtAssignContext ctx) {
        return sexp("StmtAssign", sub(ctx.lValExpr()), sub(ctx.expr()));
    }

    @Override
    public SExp visitStmtCall(ASLParser.StmtCallContext ctx) {
        return sexp("StmtCall", sub(ctx.qualId()), sub(ctx.exprCommaList0()));
    }

    @Override
    public SExp visitStmtReturn(ASLParser.StmtReturnContext ctx) {
        return sexp("StmtReturn", maybe(ctx.expr()) );
    }


    @Override
    public SExp visitStmtAssert(ASLParser.StmtAssertContext ctx) {
        return sexp("StmtAssert", sub(ctx.expr()));
    }

    @Override
    public SExp visitStmtUnpredictable(ASLParser.StmtUnpredictableContext ctx) {
        return sexp("StmtUnpredictable");
    }

    @Override
    public SExp visitStmtImpDef(ASLParser.StmtImpDefContext ctx) {
        return sexp("StmtImpDef", stringLit(ctx.STRING_LIT()));
    }

    @Override
    public SExp visitStmtIf(ASLParser.StmtIfContext ctx) {
        return sexp("StmtIf", sub(ctx.test), sub(ctx.thenExpr), list(subs(ctx.stmtElsIf())), maybe(ctx.elseExpr));
    }

    @Override
    public SExp visitStmtElsIf(ASLParser.StmtElsIfContext ctx) {
        return sexp("StmtElsIf", sub(ctx.expr()), sub(ctx.ifEmbed()));
    }

    @Override
    public SExp visitStmtCase(ASLParser.StmtCaseContext ctx) {
        return sexp("StmtCase", sub(ctx.expr()), list(subs(ctx.caseAlt())));
    }

    @Override
    public SExp visitStmtFor(ASLParser.StmtForContext ctx) {
        return sexp("StmtFor",
                id(ctx.id()),
                sub(ctx.begin),
                atom(ctx.direction.getText()),
                sub(ctx.end),
                sub(ctx.indentedBlock()));
    }

    @Override
    public SExp visitStmtWhile(ASLParser.StmtWhileContext ctx) {
        return sexp("StmtWhile",
                sub(ctx.expr()),
                sub(ctx.indentedBlock()));
    }

    @Override
    public SExp visitStmtRepeat(ASLParser.StmtRepeatContext ctx) {
        return sexp("StmtRepeat",
                sub(ctx.indentedBlock()),
                sub(ctx.expr()));
    }

    @Override
    public SExp visitStmtThrow(ASLParser.StmtThrowContext ctx) {
        return sexp("StmtThrow",
                id(ctx.id()));
    }

    @Override
    public SExp visitStmtUndefined(ASLParser.StmtUndefinedContext ctx) {
        return sexp("StmtUndefined");
    }

    @Override
    public SExp visitStmtSee(ASLParser.StmtSeeContext ctx) {
        return sexp("StmtSee", atomq(ctx.SEE_TOK().getText().replace("\"", "")));
    }

    @Override
    public SExp visitInstructions(ASLParser.InstructionsContext ctx) {
        return sexp("Instructions", subs(ctx.instruction()));
    }

    @Override
    public SExp visitStmtDefEnum(ASLParser.StmtDefEnumContext ctx) {
        return sexp("StmtDefEnum", id(ctx.id()), sub(ctx.identifierCommaList0()));
    }

    @Override
    public SExp visitId(ASLParser.IdContext ctx) {
        return atom(ctx.getText());
    }

    @Override
    public SExp visitStmtTry(ASLParser.StmtTryContext ctx) {
        return sexp("StmtTry",
                sub(ctx.id()),
                sub(ctx.indentedBlock()),
                list(subs(ctx.catchAlt())));
    }

    @Override
    public SExp visitCatchAltWhen(ASLParser.CatchAltWhenContext ctx) {
        return sexp("CatchWhen", sub(ctx.expr()), sub(ctx.indentedBlock()));
    }

    @Override
    public SExp visitCatchAltOtherwise(ASLParser.CatchAltOtherwiseContext ctx) {
        return sexp("CatchOtherwise", sub(ctx.indentedBlock()));
    }

    @Override
    public SExp visitCaseAltWhen(ASLParser.CaseAltWhenContext ctx) {
        return sexp("CaseAltWhen", list(subs(ctx.casePattern())), maybe(ctx.expr()), sub(ctx.blockOrEmbed0()));
    }

    @Override
    public SExp visitCaseAltOtherwise(ASLParser.CaseAltOtherwiseContext ctx) {
        return sexp("CaseAltOtherwise", sub(ctx.blockOrEmbed0()));
    }

    @Override
    public SExp visitCasePatternNat(ASLParser.CasePatternNatContext ctx) {
        return sexp("CasePatternNat", nat(ctx.NAT_LIT()));
    }

    @Override
    public SExp visitCasePatternHex(ASLParser.CasePatternHexContext ctx) {
        return sexp("CasePatternHex", hex(ctx.HEX_LIT()));
    }

    @Override
    public SExp visitCasePatternBin(ASLParser.CasePatternBinContext ctx) {
        return sexp("CasePatternBin", bin(ctx.BIN_LIT()));
    }

    @Override
    public SExp visitCasePatternMask(ASLParser.CasePatternMaskContext ctx) {
        return sexp("CasePatternMask", mask(ctx.MASK_LIT()));
    }

    @Override
    public SExp visitCasePatternBind(ASLParser.CasePatternBindContext ctx) {
        return sexp("CasePatternIdentifier", id(ctx.id()));
    }

    @Override
    public SExp visitCasePatternIgnore(ASLParser.CasePatternIgnoreContext ctx) {
        return sexp("CasePatternIgnore");
    }

    @Override
    public SExp visitCasePatternTuple(ASLParser.CasePatternTupleContext ctx) {
        return sexp("CasePatternTuple", list(subs(ctx.children)));
    }

    @Override
    public SExp visitLValIgnore(ASLParser.LValIgnoreContext ctx) {
        return sexp("LValIgnore");
    }

    @Override
    public SExp visitLValVarRef(ASLParser.LValVarRefContext ctx) {
        return sexp("LValVarRef", sub(ctx.qualId()));
    }

    @Override
    public SExp visitLValMember(ASLParser.LValMemberContext ctx) {
        return sexp("LValMember", sub(ctx.lValExpr()), id(ctx.id()));
    }

    @Override
    public SExp visitLValMemberArray(ASLParser.LValMemberArrayContext ctx) {
        return sexp("LValMemberArray", sub(ctx.lValExpr()), sub(ctx.identifierCommaList1()));
    }

    @Override
    public SExp visitLValArrayIndex(ASLParser.LValArrayIndexContext ctx) {
        return sexp("LValArrayIndex", sub(ctx.lValExpr()), list(subs(ctx.slice())));
    }

    @Override
    public SExp visitLValSliceOf(ASLParser.LValSliceOfContext ctx) {
        return sexp("LValSliceOf", sub(ctx.lValExpr()), sub(ctx.sliceCommaList1()));
    }

    @Override
    public SExp visitLValArray(ASLParser.LValArrayContext ctx) {
        return sexp("LValArray", list(subs(ctx.lValExpr())));
    }

    @Override
    public SExp visitLValTuple(ASLParser.LValTupleContext ctx) {
        return sexp("LValTuple", list(subs(ctx.lValExpr())));
    }

    @Override
    public SExp visitLValMemberBits(ASLParser.LValMemberBitsContext ctx) {
        return sexp("LValMemberBits", sub(ctx.lValExpr()), sub(ctx.identifierCommaList1()));
    }

    @Override
    public SExp visitLValSlice(ASLParser.LValSliceContext ctx) {
        return sexp("LValSlice", list(subs(ctx.lValExpr())));
    }


    // -- EXPRESSIONS -----------------------------------------------
    @Override
    public SExp visitExprLitString(ASLParser.ExprLitStringContext ctx) {
        return sexp("ExprLitString", stringLit(ctx.STRING_LIT()));
    }

    @Override
    public SExp visitExprLitNat(ASLParser.ExprLitNatContext ctx) {
        return sexp("ExprLitNat", nat(ctx.NAT_LIT()));
    }

    @Override
    public SExp visitExprLitHex(ASLParser.ExprLitHexContext ctx) {
        return sexp("ExprLitHex", hex(ctx.HEX_LIT()));
    }

    @Override
    public SExp visitExprLitReal(ASLParser.ExprLitRealContext ctx) {
        return sexp("ExprLitReal", real(ctx.REAL_LIT()));
    }

    @Override
    public SExp visitExprLitBin(ASLParser.ExprLitBinContext ctx) {
        return sexp("ExprLitBin", bin(ctx.BIN_LIT()));
    }

    @Override
    public SExp visitExprLitMask(ASLParser.ExprLitMaskContext ctx) {
        return sexp("ExprLitMask", atom(ctx.MASK_LIT().getText()));
    }

    @Override
    public SExp visitExprImpDef(ASLParser.ExprImpDefContext ctx) {
        return ctx.STRING_LIT() == null ?
            sexp("ExprImpDef", atom("Nothing"))
          : sexp("ExprImpDef", sexp("Just", stringLit(ctx.STRING_LIT())));
    }

    @Override
    public SExp visitExprSlice(ASLParser.ExprSliceContext ctx) {
        return sexp("ExprSlice", sub(ctx.expr()), sub(ctx.sliceCommaList1()));
    }

    @Override
    public SExp visitExprIndex(ASLParser.ExprIndexContext ctx) {
        return sexp("ExprIndex", sub(ctx.expr()), sub(ctx.sliceCommaList0()));
    }

    @Override
    public SExp visitExprUnOp(ASLParser.ExprUnOpContext ctx) {
        return sexp("ExprUnOp", atomq(ctx.operator.getText()), sub(ctx.expr()));
    }

    @Override
    public SExp visitExprBinOp(ASLParser.ExprBinOpContext ctx) {
        return sexp("ExprBinOp", atomq(ctx.operator.getText()), sub(ctx.operand1), sub(ctx.operand2));
    }

    @Override
    public SExp visitExprMembers(ASLParser.ExprMembersContext ctx) {
        return sexp("ExprMembers", sub(ctx.expr()), sub(ctx.identifierCommaList1()));
    }

    @Override
    public SExp visitExprInMask(ASLParser.ExprInMaskContext ctx) {
        return sexp("ExprInMask", sub(ctx.expr()), mask(ctx.MASK_LIT()));
    }

    @Override
    public SExp visitExprMemberBits(ASLParser.ExprMemberBitsContext ctx) {
        return sexp("ExprMemberBits", sub(ctx.expr()), sub(ctx.identifierCommaList1()));
    }

    @Override
    public SExp visitExprCall(ASLParser.ExprCallContext ctx) {
        return sexp("ExprCall", sub(ctx.qualId()), sub(ctx.exprCommaList0()));
    }

    @Override
    public SExp visitExprInSet(ASLParser.ExprInSetContext ctx) {
        return sexp("ExprInSet", sub(ctx.expr()), sub(ctx.set()));
    }

    @Override
    public SExp visitExprUnknown(ASLParser.ExprUnknownContext ctx) {
        return sexp("ExprUnknown");
    }

    @Override
    public SExp visitExprTuple(ASLParser.ExprTupleContext ctx) {
        return sexp("ExprTuple", sub(ctx.exprCommaList1()));
    }


    @Override
    public SExp visitExprVarRef(ASLParser.ExprVarRefContext ctx) {
        return sexp("ExprVarRef", sub(ctx.qualId()));
    }

    @Override
    public SExp visitExprIf(ASLParser.ExprIfContext ctx) {
        return sexp("ExprIf", sub(ctx.test), sub(ctx.thenExpr), list(subs(ctx.exprElsIf())), sub(ctx.elseExpr));
    }

    @Override
    public SExp visitExprElsIf(ASLParser.ExprElsIfContext ctx) {
        return sexp("ExprElsIf", sub(ctx.test), sub(ctx.result));
    }

    @Override
    public SExp visitExprMember(ASLParser.ExprMemberContext ctx) {
        return sexp("ExprMember", sub(ctx.expr()), id(ctx.id()));
    }

    @Override
    public SExp visitSliceExprLitNat(ASLParser.SliceExprLitNatContext ctx) {
        return sexp("ExprLitNat", nat(ctx.NAT_LIT()));
    }

    @Override
    public SExp visitSliceExprLitHex(ASLParser.SliceExprLitHexContext ctx) {
        return sexp("ExprLitHex", hex(ctx.HEX_LIT()));
    }

    @Override
    public SExp visitSliceExprVarRef(ASLParser.SliceExprVarRefContext ctx) {
        return sexp("ExprVarRef", sub(ctx.qualId()));
    }

    @Override
    public SExp visitSliceExprCall(ASLParser.SliceExprCallContext ctx) {
        return sexp("ExprCall", sub(ctx.qualId()), sub(ctx.exprCommaList0()));
    }

    @Override
    public SExp visitSliceExprUnOp(ASLParser.SliceExprUnOpContext ctx) {
        return sexp("ExprUnOp", atomq(ctx.operator.getText()), sub(ctx.expr()));
    }

    @Override
    public SExp visitSliceExprMember(ASLParser.SliceExprMemberContext ctx) {
        return sexp("ExprMember", sub(ctx.expr()), id(ctx.id()));
    }

    @Override
    public SExp visitSliceExprBinOp(ASLParser.SliceExprBinOpContext ctx) {
        return sexp("ExprBinOp", atomq(ctx.operator.getText()), sub(ctx.operand1), sub(ctx.operand2));
    }

    @Override
    public SExp visitSliceExprIf(ASLParser.SliceExprIfContext ctx) {
        return sexp("ExprIf", sub(ctx.test), sub(ctx.thenExpr), list(subs(ctx.exprElsIf())), sub(ctx.elseExpr));
    }

    @Override
    public SExp visitSliceOffset(ASLParser.SliceOffsetContext ctx) {
        return sexp("SliceOffset", sub(ctx.base), sub(ctx.count));
    }

    @Override
    public SExp visitSliceRange(ASLParser.SliceRangeContext ctx) {
        return sexp("SliceRange", sub(ctx.begin), sub(ctx.end));
    }

    @Override
    public SExp visitSliceSingle(ASLParser.SliceSingleContext ctx) {
        return sexp("SliceSingle", sub(ctx.expr()));
    }

    @Override
    public SExp visitSetElementRange(ASLParser.SetElementRangeContext ctx) {
        return sexp("SetElementRange", sub(ctx.begin), sub(ctx.end));
    }

    @Override
    public SExp visitSetElementSingle(ASLParser.SetElementSingleContext ctx) {
        return sexp("SetElementSingle", sub(ctx.expr()));
    }

    @Override
    public SExp visitSet(ASLParser.SetContext ctx) {
        return sexp("Set", list(subs(ctx.setElement())));
    }

    @Override
    public SExp visitSliceCommaList1(ASLParser.SliceCommaList1Context ctx) {
        return list(subs(ctx.slice()));
    }

    @Override
    public SExp visitSliceCommaList0(ASLParser.SliceCommaList0Context ctx) {
        return list(subs(ctx.slice()));
    }

    @Override
    public SExp visitExprCommaList1(ASLParser.ExprCommaList1Context ctx) {
        return list(subs(ctx.expr()));
    }

    @Override
    public SExp visitExprCommaList0(ASLParser.ExprCommaList0Context ctx) {
        return list(subs(ctx.expr()));
    }
}
