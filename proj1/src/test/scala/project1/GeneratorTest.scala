package project1

import java.io._
import org.scalatest._

// Define the stream method
trait TestOutput {
  val out = new ByteArrayOutputStream
  val pOut = new PrintWriter(out, true)
  def stream = pOut
  def emitCode(ast: Exp): Unit

  def code(ast: Exp) = {
    emitCode(ast)
    out.toString.stripLineEnd
  }
}

class StackGeneratorTest extends FunSuite {

  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  // Function Helper for StackASMGenerator
  def testStackASMGenerator(ast: Exp, res: Int) = {
    val gen = new StackASMGenerator with TestOutput

    val code = gen.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("Single Digit:2") {
    testStackASMGenerator(Lit(2), 2)
  }

  test("Plus Two digits:2+3") {
    testStackASMGenerator(Plus(Lit(2),Lit(3)), 5)
  }

  test("Minus Two digits:2-3") {
    testStackASMGenerator(Minus(Lit(2),Lit(3)), -1)
  }

  test("Multiply Two digits:2*3") {
    testStackASMGenerator(Times(Lit(2),Lit(3)), 6)
  }

  test("Divide Two digits:42/6") {
    testStackASMGenerator(Div(Lit(42),Lit(6)), 7)
  }

  test("Complext mul/div/add/op:1+5*8") {
    testStackASMGenerator(Plus(Lit(1),Times(Lit(5),Lit(8))), 41)
  }

  test("Complext mul/div/add/op:1+5*8/4") {
    testStackASMGenerator(Plus(Lit(1),Div(Times(Lit(5),Lit(8)),Lit(4))), 11)
  }
}

class RegGeneratorTest extends FunSuite {

  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  // Function Helper for StackASMGenerator
  def testRegASMGenerator(ast: Exp, res: Int) = {
    val gen = new RegASMGenerator with TestOutput

    val code = gen.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }


  test("Single Digit:2") {
    testRegASMGenerator(Lit(2), 2)
  }

  test("Plus Two digits:2+3") {
    testRegASMGenerator(Plus(Lit(2),Lit(3)), 5)
  }

  test("Minus Two digits:2-3") {
    testRegASMGenerator(Minus(Lit(2),Lit(3)), -1)
  }

  test("Multiply Two digits:2*3") {
    testRegASMGenerator(Times(Lit(2),Lit(3)), 6)
  }

  test("Divide Two digits:42/6") {
    testRegASMGenerator(Div(Lit(42),Lit(6)), 7)
  }

  test("Complext mul/div/add/op:1+5*8") {
    testRegASMGenerator(Plus(Lit(1),Times(Lit(5),Lit(8))), 41)
  }

  test("Complext mul/div/add/op:1+5*8/4") {
    testRegASMGenerator(Plus(Lit(1),Div(Times(Lit(5),Lit(8)),Lit(4))), 11)
  }
}
