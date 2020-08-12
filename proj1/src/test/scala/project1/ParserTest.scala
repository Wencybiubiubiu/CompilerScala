package project1

import java.io._
import org.scalatest._

class ParserTest extends FunSuite {

  def reader(src: String) = new BaseReader(src.iterator, '\u0000')
  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  test("SingleDigit") {
    val gen = new SingleDigitParser(reader("4"))
    val ast = gen.parseCode

    assert(ast == Lit(4), "Invalid result")
  }

  // Function Helper for SingleAddOpParser
  def testSingleAdd(op: String, res: Exp) = {
    val gen = new SingleAddOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("SingleAddopAdd") {
    testSingleAdd("1+1", Plus(Lit(1),Lit(1)))
  }

  // Function Helper for MultipleAddOpParser
  def testMultipleAdd(op: String, res: Exp) = {
    val gen = new MultipleAddOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("MultipleAddopAdd") {
    testMultipleAdd("1", Lit(1))
    testMultipleAdd("1+2", Plus(Lit(1), Lit(2)))
    testMultipleAdd("1+2+3", Plus(Plus(Lit(1), Lit(2)),Lit(3)))
  }

  test("MultipleMinusopMinus") {
    testMultipleAdd("1", Lit(1))
    testMultipleAdd("1-2", Minus(Lit(1), Lit(2)))
    testMultipleAdd("1-2-3", Minus(Minus(Lit(1), Lit(2)),Lit(3)))
  }

  test("MultipleAddopMinus") {
    testMultipleAdd("1", Lit(1))
    testMultipleAdd("3+1-2", Minus(Plus(Lit(3), Lit(1)),Lit(2)))
    testMultipleAdd("1-2+3", Plus(Minus(Lit(1), Lit(2)),Lit(3)))
  }

  // Function Helper for ArithOpParser
  def testArith(op: String, res: Exp) = {
    val gen = new ArithOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("ArithMultipleAddop") {
    testArith("1", Lit(1))
    testArith("3+1*2", Plus(Lit(3),Times(Lit(1), Lit(2))))
    testArith("1*3+2", Plus(Times(Lit(1), Lit(3)),Lit(2)))
    testArith("8*2-9*3", Minus(Times(Lit(8), Lit(2)),Times(Lit(9),Lit(3))))
    testArith("8*2+9*3", Plus(Times(Lit(8), Lit(2)),Times(Lit(9),Lit(3))))
  }

  test("ArithDivideAddop") {
    testArith("1", Lit(1))
    testArith("3+4/2", Plus(Lit(3),Div(Lit(4), Lit(2))))
    testArith("8/2-9", Minus(Div(Lit(8), Lit(2)),Lit(9)))
    testArith("8/2-9/3", Minus(Div(Lit(8), Lit(2)),Div(Lit(9),Lit(3))))
    testArith("8/2+9/3", Plus(Div(Lit(8), Lit(2)),Div(Lit(9),Lit(3))))
  }

  test("ArithDivideMulAddop") {
    testArith("8*2-9/3", Minus(Times(Lit(8), Lit(2)),Div(Lit(9),Lit(3))))
    testArith("8/2+9*3", Plus(Div(Lit(8), Lit(2)),Times(Lit(9),Lit(3))))
  }

  // Function Helper for ArithParOpParser
  def testArithPar(op: String, res: Exp) = {
    val gen = new ArithParOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("ArithPar") {
    testArithPar("-1+3*(5-8)", Plus(Minus(Lit(0),Lit(1)),Times(Lit(3),Minus(Lit(5),Lit(8)))))
    testArithPar("(-2+3)/5-2*(-4)", Minus(Div(Plus(Minus(Lit(0),Lit(2)),Lit(3)), Lit(5)),Times(Lit(2),Minus(Lit(0),Lit(4)))))
  }
}
