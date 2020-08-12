package project3

import org.scalatest._
import java.io.{ByteArrayOutputStream, PrintWriter}

// Define the stream method
trait TestOutput {
  import Language._

  val out = new ByteArrayOutputStream
  val pOut = new PrintWriter(out, true)
  def stream = pOut
  def emitCode(ast: Exp): Unit

  def code(ast: Exp) = {
    emitCode(ast)
    out.toString.stripLineEnd
  }
}

class CompilerTest extends FunSuite {
  import Language._

  def runner(src: String) = new ASMRunner(src)

  def testCompiler(ast: Exp, res: Int) = {
    val interpreter = new X86Compiler with TestOutput

    val code = interpreter.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("arithm") {
    testCompiler(LetRec(Nil, Lit(-21)), -21)
    testCompiler(LetRec(Nil, Prim("-", List(Lit(10), Lit(2)))), 8)
  }

  test("binary1") {
    testCompiler(LetRec(Nil,Prim("==",List(Lit(0), Lit(1)))), 0)
  }

  test("binary2") {
    testCompiler(LetRec(Nil,Prim(">",List(Lit(0), Lit(1)))), 0)
  }

  test("binary3") {
    testCompiler(LetRec(Nil,Prim("<",List(Lit(0), Lit(1)))), 1)
  }

  test("binary4") {
    testCompiler(LetRec(Nil,Prim(">=",List(Lit(2), Lit(1)))), 1)
  }

  test("binary5") {
    testCompiler(LetRec(Nil,Prim("<=",List(Lit(0), Lit(1)))), 1)
  }
}
