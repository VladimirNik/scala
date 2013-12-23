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
  with PackagePrintTests

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
  
  //new foo
  @Test def testNewExpr1 = assertPrintedCode("new foo()")
  
  //new foo { test }
  @Test def testNewExpr2 = assertPrintedCode("""
{
  final class $anon extends foo {
    test
  };
  new $anon()
}""")
    
  //new foo[t]
  @Test def testNewExpr3 = assertPrintedCode("new foo[t]()")
  
  //new foo(x)
  @Test def testNewExpr4 = assertPrintedCode("new foo(x)")
    
  //new foo[t](x)
  @Test def testNewExpr5 = assertPrintedCode("new foo[t](x)")
    
  //new foo[t](x) { (); () }
  @Test def testNewExpr6 = assertPrintedCode("""
{
  final class $anon extends foo[t](x) {
    ();
    ()
  };
  new $anon()
}""")
    
  //new foo with bar
  @Test def testNewExpr7 = assertPrintedCode("""
{
  final class $anon extends foo with bar;
  new $anon()
}""")
    
  //new { anonymous }
  @Test def testNewExpr8 = assertPrintedCode("""
{
  final class $anon {
    anonymous
  };
  new $anon()
}""")
    
  //new { val early = 1 } with Parent[Int] { body }
  @Test def testNewExpr9 = assertPrintedCode("""
{
  final class $anon extends {
    val early = 1
  } with Parent[Int] {
    body
  };
  new $anon()
}""")
    
  //new Foo { self => }
  @Test def testNewExpr10 = assertPrintedCode("""
{
  final class $anon extends Foo { self =>
    
  };
  new $anon()
}""")
}

trait ClassPrintTests {
  @Test def testClass = assertPrintedCode("class *")
  
  @Test def testClassWithBody = assertPrintedCode("""
class X {
  def y = "test"
}
""")
  
  //fix - remove ();
  //@Test def testClassWithConstr1 = assertPrintedCode("""
  //class X(val z: Int) {
  //  def this() = {
  //    this(5);
  //    ()
  //  }
  //}
  //""")

  @Test def testClassWithPublicParams = assertPrintedCode("class X(val x: Int, val s: String)")
  
  //remove private[this] val for classes arguments
  @Test def testClassWithParams1 = assertPrintedCode("class X(private[this] val x: Int, private[this] val s: String)")
  
  @Test def testClassWithParams2 = assertPrintedCode("class X protected (val x: Int, val s: String)")
  
  @Test def testClassWithParams3 = assertPrintedCode("class X(var x: Int)")
  
  @Test def testClassWithParams4 = assertPrintedCode("class X(var x: Int*)")
  
  @Test def testClassWithByNameParam = assertPrintedCode("class X(private[this] val x: => Int)")
  
  @Test def testClassWithDefault = assertPrintedCode("class X(var x: Int = 5)")
  
  @Test def testClassWithParams5 = assertPrintedCode("class X(protected[zzz] var x: Int)")
  
  @Test def testClassWithParams6 = assertPrintedCode("class X(override var x: Int) extends F(x) with E(x)")
  
  @Test def testClassWithParams7 = assertPrintedCode("class X(val y: Int)()(var z: Double)")
  
  @Test def testClassWithImplicitParams = assertPrintedCode("class X(var i: Int)(implicit val d: Double, var f: Float)")
  
  @Test def testAbstractClass = assertPrintedCode("abstract class X(protected[zzz] var x: Int)")
  
  @Test def testCaseClassWithParams = assertPrintedCode("case class X(val x: Int, val s: String)")
  
  @Test def testCaseClassWithBody = assertPrintedCode("""
case class X() {
  def y = "test"
}
""")

  @Test def testLocalClass = assertPrintedCode("""
def test = {
  class X(var a: Int) {
    def y = "test"
  };
  new X(5)
}      
""")

  @Test def testLocalCaseClass = assertPrintedCode("""
def test = {
  case class X(var a: Int) {
    def y = "test"
  };
  new X(5)
}      
""")

  //remove val for case classes arguments
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

  @Test def testObjectWithEarly = assertPrintedCode("""
object X extends {
  val early: T = v
} with Bar
""")

  @Test def testObjectWithSelf = assertPrintedCode("""
object Foo extends Foo { self =>
  body
}
""")

  @Test def testObjectInh = assertPrintedCode("private[Y] object X extends Bar with Baz")
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

  @Test def testTraitTypeParams = assertPrintedCode("trait X[A, B]")

  //trait Foo { def foo; val bar: Baz }
  @Test def testTraitWithBody2 = assertPrintedCode("""
trait X {
  def foo: scala.Unit;
  val bar: Baz
}
""")
  
  @Test def testTraitWithInh = assertPrintedCode("trait X extends A with B")

  @Test def testTraitWithEarly1 = assertPrintedCode("""
trait X extends {
  val x: Int = 1
} with Any""")

  //fix early typeDef
  //@Test def testTraitWithEarly2 = assertPrintedCode("trait X extends { val x: Int = 0; type Foo = Bar} with Bippy")

  @Test def testTraitWithEarly3 = assertPrintedCode("""
trait X { self: Foo =>
  val x: Int = 1
}""")
}

trait ValAndDefPrintTests {
  @Test def testVal = assertPrintedCode("val * : Unit = null")
  
  @Test def testDef = assertPrintedCode("def * : Unit = null")
  
  @Test def testDefWithParams1 = assertPrintedCode("def foo(x: Int*) = null")
  
  @Test def testDefWithParams2 = assertPrintedCode("def foo(x: Int)(y: Int = 1) = null")
  
  @Test def testDefWithTypeParams = assertPrintedCode("def foo[A, B <: Bar] = null")

  @Test def testDefWithAnn1 = assertPrintedCode("@a(x) def foo = null")

  //@Foo[A,B] def foo
  @Test def testDefWithAnn2 = assertPrintedCode("@Foo[A, B]() def foo = null")
  
  //fix @Foo(a)(b) def foo
  //@Test def testDefWithAnn3 = assertPrintedCode("@Foo(a)(b) def foo")
}

trait PackagePrintTests {
  //package foo.bar { }
  @Test def testPackage1 = assertPrintedCode("""
package foo.bar {
  
}
""")
  
  @Test def testPackage2 = assertPrintedCode("""
package foo {
  class C

  object D
}
""")

  //package object foo extends a with b
  @Test def testPackage3 = assertPrintedCode("""
package foo {
  object `package` extends a with b
}
""")

  //package object foo { def foo; val x = 1 }
  @Test def testPackage4 = assertPrintedCode("""
package foo {
  object `package` {
    def foo: scala.Unit;
    val x = 1
  }
}
""")

  //fix type - package object foo extends { val x = 1; type I = Int } with Any
  //@Test def testPackage5 = assertPrintedCode("package object foo extends { val x = 1; type I = Int } with Any")
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
