package edu.colorado.csci3155.project1

object StackMachineCompiler {


    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        /* Begin Solution */
        e match {
            /* TODO: Your code here must handle the cases for Expr (see Expr.scala) */
        case  Const(f) => List(PushNumI(f))
        case  BoolConst(b) => List(PushBoolI(b))
        case  Ident(id) => List(StoreEnv(id))
        case  Plus(e1, e2) =>
        {
            val L1 = compileToStackMachineCode(e1)
            val L2 = compileToStackMachineCode(e2);
            val L3 = L1++L2++List(AddI)
            L3

        }
        case  Minus(e1, e2) => 
         {
            val L1 = compileToStackMachineCode(e1)
            val L2 = compileToStackMachineCode(e2);
            val L3 = L1++L2++List(SubI)
            L3
        }
        case  Mult(e1, e2) => 
        {
            val L1 = compileToStackMachineCode(e1)
            val L2 = compileToStackMachineCode(e2);
            val L3 = L1++L2++List(MultI)
            L3
        }
        case  Div(e1, e2) =>
        {
            val L1 = compileToStackMachineCode(e1)
            val L2 = compileToStackMachineCode(e2);
            val L3 = L1++L2++List(DivI)
            L3
        }
        case  Exp(e) =>
        {
            val L1 = compileToStackMachineCode(e)
            val L3 = L1++List(ExpI)
            L3
        }
        case  Log(e) =>
        {
            val L1 = compileToStackMachineCode(e)
            val L3 = L1++List(LogI)
            L3
        }
        case  Sine(e) => 
        {
            val L1 = compileToStackMachineCode(e)
            val L3 = L1++List(SinI)
            L3
        }
        case  Cosine(e) =>
        {
            val L1 = compileToStackMachineCode(e)
            val L3 = L1++List(CosI)
            L3
        }
        case  Geq(e1, e2) =>
        {
            val L1 = compileToStackMachineCode(e1)
            val L2 = compileToStackMachineCode(e2);
            val L3 = L1++L2++List(GeqI)
            L3
        }
        case  Eq(e1, e2) =>
        {
            val L1 = compileToStackMachineCode(e1)
            val L2 = compileToStackMachineCode(e2);
            val L3 = L1++L2++List(EqI)
            L3
        }
        case And(e1, e2) =>
        {
            val L1 = compileToStackMachineCode(e1)
            val L2 = compileToStackMachineCode(e2)
            List(L1, List(CSkipI(L2.length+1)), L2, List(SkipI(1), PushBoolI(false))).flatten
        }
        case  Or (e1, e2) =>
        {
            val L1 = compileToStackMachineCode(e1)
            val L2 = compileToStackMachineCode(e2)
            List(L1, List(CSkipI(2), PushBoolI(true), SkipI(L2.length)), L2).flatten
        }
        case  Not(e) =>
        {
            val L1 = compileToStackMachineCode(e)
            val L3 = L1++List(NotI)
            L3

        }
        case  IfThenElse(cond, tExpr, elseExpr) =>
        {
            val L0 = compileToStackMachineCode(cond)
            val L1 = compileToStackMachineCode(tExpr)
            val L2 = compileToStackMachineCode(elseExpr)
            val Concat = L0 ++ List(CSkipI(L1.length +1)) ++ L1 ++ List(SkipI(L2.length)) ++ L2
            Concat
        }
        case  Let(ident, e1, e2) =>
        {
            val L1 = compileToStackMachineCode(e1)
            val L2 = compileToStackMachineCode(e2)
            val conCat = L1 ++ (List(LoadEnv(ident))) ++ L2 ++ (List(PopEnv))
            conCat 
        }
        case _ => throw new IllegalArgumentException(s"I do not handle $e")
        }
        /* End Solution */
    }
}
