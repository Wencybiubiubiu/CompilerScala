package project3

import org.scalatest._

class SemanticAnalyzerTest extends FunSuite {
  import Language._

  def astTypeEquals(ast: Exp, tsa: Exp): Boolean = ast == tsa && ast.tp == tsa.tp && { (ast, tsa) match {
  //def astTypeEquals(ast: Exp, tsa: Exp): Boolean = if(ast.tp != tsa.tp) throw new Exception(s"$ast,$tsa,$ast.tp,$tsa.tp") { (ast, tsa) match {
  //def astTypeEquals(ast: Exp, tsa: Exp): Boolean = ast == tsa && ast.tp == tsa.tp && { (ast, tsa) match {
    case (Prim(_, args), Prim(_, sgra))=>
      println("Prim")
      println(ast.tp == tsa.tp)
      println(ast,tsa)
      println(ast.tp,tsa.tp)
      (args zip sgra) forall { case (arg, gra) => astTypeEquals(arg, gra) }
    case (Let(_, _, a, b), Let(_, _, c, d)) =>
      println("Let")
      println(ast.tp == tsa.tp)
      println(ast,tsa)
      println(ast.tp,tsa.tp)
      astTypeEquals(a, c) && astTypeEquals(b, d)
    case (If(cond, tBranch, eBranch), If(cond1, tBranch1, eBranch1)) =>
      println("If")
      println(ast.tp == tsa.tp)
      println(ast,tsa)
      println(ast.tp,tsa.tp)
      astTypeEquals(cond, cond1) && astTypeEquals(tBranch, tBranch1) && astTypeEquals(eBranch, eBranch1)
    case (VarDec(_, _, a, b), VarDec(_, _, c, d)) =>
      println("VarDec")
      println(ast.tp == tsa.tp)
      println(ast,tsa)
      println(ast.tp,tsa.tp)
      astTypeEquals(a, c) && astTypeEquals(b, d)
    case (VarAssign(_, rhs), VarAssign(_, shr)) =>
      println("VarAssign")
      println(ast.tp == tsa.tp)
      println(ast,tsa)
      println(ast.tp,tsa.tp)
      astTypeEquals(rhs, shr)
    case (While(cond, tBranch, eBranch), While(cond1, tBranch1, eBranch1)) =>
      println("While")
      println(ast.tp == tsa.tp)
      println(ast,tsa)
      println(ast.tp,tsa.tp)
      astTypeEquals(cond, cond1) && astTypeEquals(tBranch, tBranch1) && astTypeEquals(eBranch, eBranch1)
    case (FunDef(_, _, _, fbody), FunDef(_, _, _, fbody1)) =>
      println("FunDef")
      println(ast.tp == tsa.tp)
      println(ast,tsa)
      println(ast.tp,tsa.tp)
      astTypeEquals(fbody, fbody1)
    case (LetRec(funs, body), LetRec(funs1, body1)) =>
      println("LetRec")
      println(ast.tp == tsa.tp)
      println(ast,tsa)
      println(ast.tp,tsa.tp)
      ((funs zip funs1) forall { case (arg, gra) => astTypeEquals(arg, gra) }) && astTypeEquals(body, body1)
    case (App(fun, args), App(fun1, args1)) =>
      println("App")
      println(ast.tp == tsa.tp)
      println(ast,tsa)
      println(ast.tp,tsa.tp)
      ((args zip args1) forall { case (arg, gra) => astTypeEquals(arg, gra) }) && astTypeEquals(fun, fun1)
    case (ArrayDec(size, _), ArrayDec(size1, _)) =>
      println("ArrayDec")
      println(ast.tp == tsa.tp)
      println(ast,tsa)
      println(ast.tp,tsa.tp)
      astTypeEquals(size, size1)
    case _ =>
      println("true")
      println(ast.tp == tsa.tp)
      println(ast,tsa)
      println(ast.tp,tsa.tp)
      true
  }}

  def testSemanticAnalyzer(ast: Exp, tsa: Exp, nWarning: Int, nError: Int) = {
    val fakeParser = new Parser(null) {
      override def error(msg: String, pos: Position) = {}
      override def warn(msg: String, pos: Position) = {}
    }

    val analyzer = new SemanticAnalyzer(fakeParser)

    val (tast, w, e) = analyzer.run(ast)
    assert(w == nWarning, "Incorrect number of Warnings")
    assert(e == nError, "Incorrect number of Errors")
    println(tast)
    println(tast.tp)
    println(tsa)
    println(tsa.tp)
    assert(astTypeEquals(tast, tsa), "AST does not have correct type")
  }

  test("NoErrorNoWarning1") {
    testSemanticAnalyzer(Lit(1), Lit(1).withType(IntType), 0, 0)
  }

  test("NoErrorNoWarning2") {
    testSemanticAnalyzer(Prim("+", List(Lit(1), Lit(2))), Prim("+", List(Lit(1).withType(IntType), Lit(2).withType(IntType))).withType(IntType), 0, 0)
  }

  test("Prim1") {
    testSemanticAnalyzer(Prim("*", List(Lit(1), Lit(2))), Prim("*", List(Lit(1).withType(IntType), Lit(2).withType(IntType))).withType(IntType), 0, 0)
  }


  test("Prim2") {
    testSemanticAnalyzer(Prim("-", List(Lit(1), Lit(2))), Prim("-", List(Lit(1).withType(IntType), Lit(2).withType(IntType))).withType(IntType), 0, 0)
  }

  test("Prim3") {
    testSemanticAnalyzer(Prim("+",List(Lit(1),Prim("-", List(Lit(1), Lit(2))))), Prim("+",List(Lit(1).withType(IntType),Prim("-", List(Lit(1).withType(IntType), Lit(2).withType(IntType))).withType(IntType))).withType(IntType), 0, 0)
  }

  test("1ErrorNoWarning3") {
    testSemanticAnalyzer(VarAssign("x",Lit(1)), VarAssign("x",Lit(1).withType(IntType)).withType(IntType), 0, 1)
  }

  test("NoErrorNoWarning4") {
    testSemanticAnalyzer(Let("x",IntType,Lit(3),Lit(2)),Let("x",IntType,Lit(3).withType(IntType),Lit(2).withType(IntType)).withType(IntType), 0, 0)
  }

  test("1ErrorNoWarning5") {
    testSemanticAnalyzer(Prim("+",List(Ref("x"), Lit(2))),Prim("+",List(Ref("x").withType(IntType), Lit(2).withType(IntType))).withType(IntType), 0, 1)
  }

  test("NoErrorNoWarning6") {
    testSemanticAnalyzer(Let("x",UnknownType,Lit(3),Prim("+",List(Ref("x"), Lit(2)))),
      Let("x",IntType,Lit(3).withType(IntType),Prim("+",List(Ref("x").withType(IntType), Lit(2).withType(IntType))).withType(IntType)).withType(IntType), 0, 0)
  }

  test("NoErrorNoWarning7") {
    testSemanticAnalyzer(VarDec("x",UnknownType,Lit(3),Prim("+",List(Ref("x"), Lit(2)))),
      VarDec("x",IntType,Lit(3).withType(IntType),Prim("+",List(Ref("x").withType(IntType), Lit(2).withType(IntType))).withType(IntType)).withType(IntType), 0, 0)
  }

  test("ref_exp1") {
    testSemanticAnalyzer(Ref("x"),Ref("x").withType(IntType), 0, 1)
  }

  test("ref_exp2") {
    testSemanticAnalyzer(Ref("x"),Ref("x").withType(UnknownType), 0, 1)
  }

  test("if_else_unit") {
    testSemanticAnalyzer(Let("dummy$2",UnknownType,Let("dummy$1",UnknownType,Lit(0),If(Prim("==",List(Lit(2), Lit(1))),Lit(2),Lit(()))),Lit(1)),
      Let("dummy$2",UnitType,Let("dummy$1",IntType,Lit(0).withType(IntType),If(Prim("==",List(Lit(2).withType(IntType), Lit(1).withType(IntType))).withType(BooleanType), Let("dummy_1",IntType,Lit(2).withType(IntType),Lit(()).withType(UnitType)).withType(UnitType),Lit(()).withType(UnitType)).withType(UnitType)).withType(UnitType), Lit(1).withType(IntType)).withType(IntType), 0, 0)

  }

  test("while") {
    testSemanticAnalyzer(VarDec("x",UnknownType,Lit(3),While(Prim(">",List(Ref("x"), Lit(0))),VarAssign("x",Prim("-",List(Ref("x"), Lit(1)))),Prim("-",List(Ref("x"), Lit(1))))),
      VarDec("x",IntType,Lit(3).withType(IntType),While(Prim(">",List(Ref("x").withType(IntType), Lit(0).withType(IntType))).withType(BooleanType),Let("dummy",IntType,VarAssign("x",Prim("-",List(Ref("x").withType(IntType), Lit(1).withType(IntType))).withType(IntType)).withType(IntType),Lit(()).withType(UnitType)).withType(UnitType),Prim("-",List(Ref("x").withType(IntType), Lit(1).withType(IntType))).withType(IntType)).withType(IntType)).withType(IntType), 0, 0)

  }


}
