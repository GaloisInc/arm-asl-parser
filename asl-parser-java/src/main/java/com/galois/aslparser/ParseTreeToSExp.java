package com.galois.aslparser;


import com.galois.aslparser.gen.ASLBaseVisitor;
import com.galois.aslparser.gen.ASLParser;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

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

    private SExp id(TerminalNode identifier) {
        return atomq(identifier.getText());
    }

    private SExp list(List<SExp> pt) {
        return sexp("list", pt);
    }

    private SExp nat(TerminalNode term) {
        return atom(term.getText());
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
        return atomq(bin_lit.getText().replace("'", ""));
    }

    private SExp mask(TerminalNode mask_lit) {
        return atomq(mask_lit.getText().replace("'", ""));
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
    public SExp visitBlockOrEmbed1(ASLParser.BlockOrEmbed1Context ctx) {
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
        return list(ctx.IDENTIFIER().stream().map(this::id).collect(Collectors.toList()));
    }

    @Override
    public SExp visitIdentifierCommaList1(ASLParser.IdentifierCommaList1Context ctx) {
        return list(ctx.IDENTIFIER().stream().map(this::id).collect(Collectors.toList()));
    }

    @Override
    public SExp visitSymDecl(ASLParser.SymDeclContext ctx) {
        return sexp("SymDecl", id(ctx.IDENTIFIER()), sub(ctx.type()));
    }

    @Override
    public SExp visitQualIdUnqualified(ASLParser.QualIdUnqualifiedContext ctx) {
        return sexp("QualifiedIdentifier", atom("Any"), id(ctx.IDENTIFIER()));
    }

    @Override
    public SExp visitQualIdAArch32(ASLParser.QualIdAArch32Context ctx) {
        return sexp("QualifiedIdentifier", atom("AArch32"), id(ctx.IDENTIFIER()));
    }

    @Override
    public SExp visitQualIdAArch64(ASLParser.QualIdAArch64Context ctx) {
        return sexp("QualifiedIdentifier", atom("AArch64"), id(ctx.IDENTIFIER()));
    }

    @Override
    public SExp visitDefinitions(ASLParser.DefinitionsContext ctx) {
        return sexp("AslDefinitions", subs(ctx.definition()));
    }

    @Override
    public SExp visitDefTypeBuiltin(ASLParser.DefTypeBuiltinContext ctx) {
        return sexp("DefTypeBuiltin", id(ctx.IDENTIFIER()));
    }

    @Override
    public SExp visitDefTypeAbstract(ASLParser.DefTypeAbstractContext ctx) {
        return sexp("DefTypeAbstract", id(ctx.IDENTIFIER()));
    }

    @Override
    public SExp visitDefTypeAlias(ASLParser.DefTypeAliasContext ctx) {
        return sexp("DefTypeAlias", id(ctx.IDENTIFIER()), sub(ctx.type()));
    }

    @Override
    public SExp visitDefTypeStruct(ASLParser.DefTypeStructContext ctx) {
        return sexp("DefTypeStruct", sub(ctx.qualId()), sub(ctx.symDeclCommaList()));
    }

    @Override
    public SExp visitDefTypeEnum(ASLParser.DefTypeEnumContext ctx) {
        return sexp("DefTypeEnum", id(ctx.IDENTIFIER()), sub(ctx.identifierCommaList0()));
    }

    @Override
    public SExp visitDefVariable(ASLParser.DefVariableContext ctx) {
        return sexp("VariableDefinition", sub(ctx.qualId()), sub(ctx.type()));
    }

    @Override
    public SExp visitDefConstant(ASLParser.DefConstantContext ctx) {
        return sexp("ConstantDefinition", id(ctx.IDENTIFIER()), sub(ctx.type()), sub(ctx.expr()));
    }

    @Override
    public SExp visitDefArray(ASLParser.DefArrayContext ctx) {
        return sexp("ArrayDefinition", id(ctx.IDENTIFIER()), sub(ctx.type()), sub(ctx.ixType()));
    }

    @Override
    public SExp visitDefCallable(ASLParser.DefCallableContext ctx) {
        return sexp("CallableDefinition",
                sub(ctx.qualId()),
                sub(ctx.symDeclCommaList()),
                maybe(ctx.returnType()),
                maybe(ctx.indentedBlock()));
    }

    @Override
    public SExp visitDefGetter(ASLParser.DefGetterContext ctx) {
        return sexp("GetterDefinition",
                sub(ctx.qualId()),
                maybe(ctx.symDeclCommaList()),
                sub(ctx.returnType()),
                maybe(ctx.indentedBlock()));
    }

    @Override
    public SExp visitDefSetter(ASLParser.DefSetterContext ctx) {
        return sexp("SetterDefinition",
                sub(ctx.qualId()),
                list(subs(ctx.setterArg())),
                sub(ctx.symDecl()),
                maybe(ctx.indentedBlock()));
    }

    @Override
    public SExp visitSetterRefArg(ASLParser.SetterRefArgContext ctx) {
        return sexp("SetterArg", sub(ctx.type()), atom("Reference"));
    }

    @Override
    public SExp visitSetterValArg(ASLParser.SetterValArgContext ctx) {
        return sexp("SetterArg", sub(ctx.type()), atom("Value"));
    }

    @Override
    public SExp visitTypeRef(ASLParser.TypeRefContext ctx) {
        return sexp("TypeRef", sub(ctx.qualId()) );
    }

    @Override
    public SExp visitTypeIndexed(ASLParser.TypeIndexedContext ctx) {
        return sexp("TypeFun", id(ctx.IDENTIFIER()), sub(ctx.expr()));
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
    public SExp visitIxTypeIdentifier(ASLParser.IxTypeIdentifierContext ctx) {
        return sexp("IxTypeIdentifier", id(ctx.IDENTIFIER()));
    }

    @Override
    public SExp visitIxTypeRange(ASLParser.IxTypeRangeContext ctx) {
        return sexp("IxTypeRange", sub(ctx.begin), sub(ctx.end));
    }

    @Override
    public SExp visitRegField(ASLParser.RegFieldContext ctx) {
        return sexp("RegField", id(ctx.IDENTIFIER()), list(subs(ctx.slice())));
    }

    @Override
    public SExp visitStmtVarsDecl(ASLParser.StmtVarsDeclContext ctx) {
        return sexp("StmtVarsDecl", sub(ctx.identifierCommaList0()), sub(ctx.type()));
    }

    @Override
    public SExp visitStmtVarDeclInit(ASLParser.StmtVarDeclInitContext ctx) {
        return sexp("StmtVarDeclInit", sub(ctx.symDecl()), sub(ctx.expr()));
    }

    @Override
    public SExp visitStmtConstDecl(ASLParser.StmtConstDeclContext ctx) {
        return sexp("StmtConstDel", sub(ctx.symDecl()), sub(ctx.expr()));
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
        return sexp("StmtIf", sub(ctx.test), list(subs(ctx.stmtElsIf())), maybe(ctx.elseExpr));
    }

    @Override
    public SExp visitStmtElsIf(ASLParser.StmtElsIfContext ctx) {
        return sexp("StmtElsIf", sub(ctx.expr()), sub(ctx.expr()));
    }

    @Override
    public SExp visitStmtCase(ASLParser.StmtCaseContext ctx) {
        return sexp("StmtCase", sub(ctx.expr()), list(subs(ctx.caseAlt())));
    }

    @Override
    public SExp visitStmtFor(ASLParser.StmtForContext ctx) {
        return sexp("StmtFor",
                id(ctx.IDENTIFIER()),
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
                id(ctx.IDENTIFIER()));
    }

    @Override
    public SExp visitStmtUndefined(ASLParser.StmtUndefinedContext ctx) {
        return sexp("StmtUndefined");
    }

    @Override
    public SExp visitStmtSeeExpr(ASLParser.StmtSeeExprContext ctx) {
        return sexp("StmtSeeExpr", sub(ctx.expr()));
    }

    @Override
    public SExp visitStmtSeeStrLit(ASLParser.StmtSeeStrLitContext ctx) {
        return sexp("StmtSeeStrLit", stringLit(ctx.STRING_LIT()));
    }

    @Override
    public SExp visitStmtTry(ASLParser.StmtTryContext ctx) {
        return sexp("StmtTry",
                sub(ctx.IDENTIFIER()),
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
        return sexp("CasePatternIdentifier", id(ctx.IDENTIFIER()));
    }

    @Override
    public SExp visitCasePatternIgnore(ASLParser.CasePatternIgnoreContext ctx) {
        return sexp("CasePatternIgnore");
    }

    @Override
    public SExp visitCasePatternTuple(ASLParser.CasePatternTupleContext ctx) {
        return sexp("CasePatternSequence", list(subs(ctx.children)));
    }

    @Override
    public SExp visitLValArray(ASLParser.LValArrayContext ctx) {
        return sexp("LValArray", list(subs(ctx.lValExpr())));
    }

    @Override
    public SExp visitLValArrayIndex(ASLParser.LValArrayIndexContext ctx) {
        return sexp("LValArrayIndex", sub(ctx.lValExpr()), list(subs(ctx.slice())));
    }

    @Override
    public SExp visitLValMemberBits(ASLParser.LValMemberBitsContext ctx) {
        return sexp("LValMemberBits", sub(ctx.lValExpr()), sub(ctx.identifierCommaList1()));
    }

    @Override
    public SExp visitLValIgnore(ASLParser.LValIgnoreContext ctx) {
        return sexp("LValIgnore");
    }

    @Override
    public SExp visitLValMemberArray(ASLParser.LValMemberArrayContext ctx) {
        return sexp("LValMemberArray", sub(ctx.lValExpr()), sub(ctx.identifierCommaList1()));
    }

    @Override
    public SExp visitLValMember(ASLParser.LValMemberContext ctx) {
        return sexp("LValMember", sub(ctx.lValExpr()), id(ctx.IDENTIFIER()));
    }

    @Override
    public SExp visitLValVarRef(ASLParser.LValVarRefContext ctx) {
        return sexp("LValVarRef", sub(ctx.qualId()));
    }

    @Override
    public SExp visitLValSliceOf(ASLParser.LValSliceOfContext ctx) {
        return sexp("LValSliceOf", sub(ctx.lValExpr()), sub(ctx.sliceCommaList1()));
    }


    @Override
    public SExp visitLValTuple(ASLParser.LValTupleContext ctx) {
        return sexp("LValTuple", list(subs(ctx.lValExpr())));
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
    public SExp visitExprImpDef(ASLParser.ExprImpDefContext ctx) {
        if(ctx.STRING_LIT() == null) {
            return sexp("ExprImpDef");
        } else {
            return sexp("ExprImpDef", stringLit(ctx.STRING_LIT()));
        }
    }

    @Override
    public SExp visitExprSlice(ASLParser.ExprSliceContext ctx) {
        return sexp("ExprSlice", sub(ctx.expr()), sub(ctx.sliceCommaList1()));
    }

    @Override
    public SExp visitExprConcat(ASLParser.ExprConcatContext ctx) {
        return sexp("ExprConcat", sub(ctx.operand1), sub(ctx.operand2));
    }

    @Override
    public SExp visitExprIndex(ASLParser.ExprIndexContext ctx) {
        return sexp("ExprIndex", sub(ctx.expr()), sub(ctx.sliceCommaList0()));
    }

    @Override
    public SExp visitExprUnOp(ASLParser.ExprUnOpContext ctx) {
        return sexp("ExprUnOp", atom(ctx.operator.getText()), sub(ctx.expr()));
    }

    @Override
    public SExp visitExprBinOp(ASLParser.ExprBinOpContext ctx) {
        return sexp("ExprBinOp", atom(ctx.operator.getText()), sub(ctx.operand1), sub(ctx.operand2));
    }

    @Override
    public SExp visitExprLitNat(ASLParser.ExprLitNatContext ctx) {
        return sexp("ExprLitNat", nat(ctx.NAT_LIT()));
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
    public SExp visitExprLitHex(ASLParser.ExprLitHexContext ctx) {
        return sexp("ExprLitHex", hex(ctx.HEX_LIT()));
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
    public SExp visitExprLitBin(ASLParser.ExprLitBinContext ctx) {
        return sexp("ExprLitBin", bin(ctx.BIN_LIT()));
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
    public SExp visitExprLitReal(ASLParser.ExprLitRealContext ctx) {
        return sexp("ExprLitReal", real(ctx.REAL_LIT()));
    }


    @Override
    public SExp visitExprVarRef(ASLParser.ExprVarRefContext ctx) {
        return sexp("ExprVarRef", sub(ctx.qualId()));
    }

    @Override
    public SExp visitExprLitMask(ASLParser.ExprLitMaskContext ctx) {
        return sexp("ExprLitMask", atom(ctx.MASK_LIT().getText()));
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
        return sexp("ExprMember", sub(ctx.expr()), id(ctx.IDENTIFIER()));
    }


    @Override
    public SExp visitSliceOffset(ASLParser.SliceOffsetContext ctx) {
        return sexp("SliceOffset", sub(ctx.base), sub(ctx.count));
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
