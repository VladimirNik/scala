package scala.reflect.internal

import org.junit.Test
import org.junit.Assert._
import scala.tools.reflect._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm}
import PrinterHelper._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class PrintersTest extends BasePrintTests
  with ClassPrintTests
  with TraitPrintTests
  with ValAndDefPrintTests
  with QuasiTreesPrintTests

object PrinterHelper {
  val toolbox = cm.mkToolBox()
  def assertPrintedCode(code: String, tree: Tree = null) = {
    val toolboxTree = 
      try{
        toolbox.parse(code)
      } catch {
        case e:scala.tools.reflect.ToolBoxError => throw new Exception(e.getMessage + ": " + code)
      }
    if (tree ne null) assertEquals("using quasiquote or given tree"+"\n", code.trim, toCode(tree))
    assertEquals("using toolbox parser", code.trim, toCode(toolboxTree))
  }
}

trait BasePrintTests {
  @Test def testIdent = assertPrintedCode("*", Ident("*"))
  
  @Test def testConstant = assertPrintedCode("\"*\"", Literal(Constant("*")))
}

trait ClassPrintTests {
  @Test def testClass = assertPrintedCode("class *")
  
  @Test def testClassWithBody = assertPrintedCode("""
class X {
  def y = "test"
}
""")
  
  @Test def testClassWithPublicParams = assertPrintedCode("class xXx(val x: Int, val s: String)")
  
  //TODO don't print private[this] val for classes arguments
  @Test def testClassWithParams = assertPrintedCode("class xXx(private[this] val x: Int, private[this] val s: String)")
  
  @Test def testCaseClassWithBody = assertPrintedCode("""
case class X() {
  def y = "test"
}
""")

  //TODO don't print val for case classes arguments
  @Test def testCaseClassWithParamsAndBody = assertPrintedCode("""
case class X(val x: Int, val s: String) {
  def y = "test"
}
""")

  @Test def testObject = assertPrintedCode("object *")
  
  @Test def testObjectWithBody = assertPrintedCode("""
object X {
  def y = "test"
}
""")
}

trait TraitPrintTests {
  @Test def testTrait = assertPrintedCode("trait *")
  
  @Test def testTraitWithBody = assertPrintedCode("""
trait X {
  def y = "test"
}
""")

  @Test def testTraitWithSelfTypeAndBody = assertPrintedCode("""
trait X { self: Order =>
  def y = "test"
}
""")

  @Test def testTraitWithSelf = assertPrintedCode("""
trait X { self =>
  def y = "test"
}
""")
}

trait ValAndDefPrintTests {
  @Test def testVal = assertPrintedCode("val * : Unit = null")
  
  @Test def testDef = assertPrintedCode("def * : Unit = null")
}

trait QuasiTreesPrintTests {  
  @Test def testQuasiIdent = assertPrintedCode("*", q"*")
  
  @Test def testQuasiVal = assertPrintedCode("val * : Unit = null", q"val * : Unit = null")
  
  @Test def testQuasiDef = assertPrintedCode("def * : Unit = null", q"def * : Unit = null")
  
  @Test def testQuasiTrait = assertPrintedCode("trait *", q"trait *")
  
  @Test def testQuasiClass = assertPrintedCode("class *", q"class *")
  
  @Test def testQuasiClassWithPublicParams = assertPrintedCode( "class xXx(val x: Int, val s: String)", q"class xXx(val x: Int, val s:String)" )
  
  //TODO don't print private[this] val for classes arguments
  @Test def testQuasiClassWithParams = assertPrintedCode("class xXx(private[this] val x: Int, private[this] val s: String)", q"class xXx(x: Int, s:String)")
  
  @Test def testQuasiObject = assertPrintedCode("object *", q"object *")
  
  @Test def testQuasiObjectWithBody = assertPrintedCode("""
object X {
  def y = "test"
}
""", q"""object X{ def y = "test" }""")

  @Test def testQuasiClassWithBody = assertPrintedCode("""
class X {
  def y = "test"
}
""", q"""class X{ def y = "test" }""")

  @Test def testQuasiTraitWithBody = assertPrintedCode("""
trait X {
  def y = "test"
}
""", q"""trait X{ def y = "test" }""")

  @Test def testQuasiTraitWithSelfTypeAndBody = assertPrintedCode("""
trait X { self: Order =>
  def y = "test"
}
""", q"""trait X{ self: Order => def y = "test" }""")

  @Test def testQuasiTraitWithSelf = assertPrintedCode("""
trait X { self =>
  def y = "test"
}
""", q"""trait X{ self => def y = "test" }""")

  @Test def testQuasiCaseClassWithBody = assertPrintedCode("""
case class X() {
  def y = "test"
}""", q"""case class X() { def y = "test" }""")

  //TODO don't print val for case classes arguments
  @Test def testQuasiCaseClassWithParamsAndBody = assertPrintedCode("""
case class X(val x: Int, val s: String) {
  def y = "test"
}""", q"""case class X(x: Int, s: String){ def y = "test" }""")
}