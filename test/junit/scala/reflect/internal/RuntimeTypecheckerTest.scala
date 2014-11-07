package scala.reflect.internal

import org.junit.Test
import org.junit.Assert._
import scala.tools.reflect._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class RuntimeTypecheckerTest extends BaseTypedTests with ClassTypedTests with TraitTypedTests
  with ValAndDefTypedTests with PackageTypedTests

object RuntimeTypecheckerHelper {
  val toolbox = cm.mkToolBox()
  val mirror = runtimeMirror(toolbox.mirror.classLoader)

  import scala.reflect.internal.Chars._
  private def normalizeEOL(resultCode: String) =
    resultCode.lines mkString s"$LF"

  def assertTypedTree(code: String, wrap: Boolean = false, printRoot: Boolean = false) = {
    def toolboxTree(tree: => Tree) = try {
        tree
      } catch {
        case e:scala.tools.reflect.ToolBoxError => throw new Exception(e.getMessage + ": " + code)
      }

    def wrapCode(source: String) = {
      val context = sm"""
      |trait PrintersContext {
      |  class baz extends scala.annotation.StaticAnnotation;
      |  class foo1[A, B] extends scala.annotation.StaticAnnotation;
      |  class foo2[A, B](a: scala.Int)(b: scala.Int) extends scala.annotation.StaticAnnotation;
      |  class foo3[Af, Bf](a: scala.Int)(b: scala.Float, c: PrintersContext.this.foo1[Af, Bf]) extends scala.annotation.StaticAnnotation;
      |  trait A1;
      |  trait B1;
      |${source.trim.lines map {"  " + _} mkString s"$LF"}
      |}"""

      if (wrap) context.trim() else source.trim
    }

//    val showRawToolbox = normalizeEOL(showRaw(typedToolboxTree, printKinds = true))
//    val showRawTypechecked = normalizeEOL(showRaw(typecheckedTree, printKinds = true))
//    
//    val showToolbox = normalizeEOL(show(typedToolboxTree, printTypes = true))
//    val showTypechecked = normalizeEOL(show(typecheckedTree, printTypes = true))
//
//    assertEquals("using showRaw" + LF, showRawToolbox, showRawTypechecked)
//    assertEquals("using show" + LF, showToolbox, showTypechecked)

    val parsedTree = toolboxTree(toolbox.parse(wrapCode(code)))
    val typedToolboxTree = toolboxTree(toolbox.typecheck(parsedTree))
    val typecheckedTree = mirror.typecheck(parsedTree)

    val showTypedToolboxTree = show(typedToolboxTree)
    val showTypecheckedTree = normalizeEOL(show(typecheckedTree))
    
    println(s"showTypedToolboxTree: $showTypedToolboxTree")
    println(s"showTypecheckedTree: $showTypecheckedTree")

    assertEquals("using toolbox and typecheckTree" + LF, showTypedToolboxTree, showTypecheckedTree)
  }

  implicit class StrContextStripMarginOps(val stringContext: StringContext) extends util.StripMarginInterpolator
}

import RuntimeTypecheckerHelper._

trait BaseTypedTests {
  @Test def testConstant1 = assertTypedTree("42")

  @Test def testConstantFloat = assertTypedTree("42.0F")

  @Test def testConstantDouble = assertTypedTree("42.0")

  @Test def testConstantLong = assertTypedTree("42L")

  @Test def testOpExpr = assertTypedTree("(5).+(4)")
  
  @Test def testName1 = assertTypedTree("class test")

  @Test def testName2 = assertTypedTree("class *")

  @Test def testName4 = assertTypedTree("class `a*`")

  @Test def testName5 = assertTypedTree("val :::: = 1")

  @Test def testName6 = assertTypedTree("val `::::t` = 1")

  @Test def testName7 = assertTypedTree("""class \/""")

  @Test def testName8 = assertTypedTree("""class \\\\""")

  @Test def testName9 = assertTypedTree("""class test_\/""")

  @Test def testName10 = assertTypedTree("""class `*_*`""")

  @Test def testName11 = assertTypedTree("""class `a_*`""")

  @Test def testName12 = assertTypedTree("""class `*_a`""")

  @Test def testName13 = assertTypedTree("""class a_a""")

  @Test def testName14 = assertTypedTree("val x$11 = 5")

  @Test def testName15 = assertTypedTree("class `[]`")

  @Test def testName16 = assertTypedTree("class `()`")

  @Test def testName17 = assertTypedTree("class `{}`")

  @Test def testName18 = assertTypedTree("class <>")

  @Test def testName19 = assertTypedTree("""class `class`""")

  @Test def testName20 = assertTypedTree("""class `test name`""")

  @Test def testIfExpr1 = assertTypedTree(sm"""
    |val a = 1
    |if (a > 1)
    |  a: Int
    |else
    |  (a.toString): String
    """)

  @Test def testIfExpr2 = assertTypedTree(sm"""
    |class A {
    |  (if (true)
    |  {
    |    false;
    |    ()
    |  }
    |else
    |  {
    |    true;
    |    ()
    |  }).toString()
    |}""")

  @Test def testIfExpr3 = assertTypedTree(sm"""
    |class A {
    |  (if (true)
    |  {
    |    false;
    |    ()
    |  }
    |else
    |  {
    |    true;
    |    ()
    |  }).toString().hashCode()
    |}""")

  //val x = true && true && false.!
  @Test def testBooleanExpr1 = assertTypedTree("val x = true.&&(true).&&(false.`unary_!`)")

  //val x = true && !(true && false)
  @Test def testBooleanExpr2 = assertTypedTree("val x = true.&&(true.&&(false).`unary_!`)")

  @Test def testNewExpr1 = assertTypedTree(
    code = sm"""
    |class foo
    |new foo()
    |""",
    wrap = true)

  @Test def testNewExpr2 = assertTypedTree(
    code = sm"""
    |class foo
    |new foo { "test" }
    |""",
    wrap = true)

  @Test def testNewExpr3 = assertTypedTree(sm"""
    |{
    |  class foo[t];
    |  new foo[scala.Int]()
    |}""")

  @Test def testNewExpr4 = assertTypedTree(sm"""
    |{
    |  class foo(x: scala.Int);
    |  val x = 5;
    |  new foo(x)
    |}""")

  @Test def testNewExpr5 = assertTypedTree(sm"""
    |{
    |  class foo[t](x: scala.Int);
    |  val x = 5;
    |  new foo[scala.Predef.String](x)
    |}""")

  //new foo[t](x) { () }
  @Test def testNewExpr6 = assertTypedTree(
    code = sm"""
    |class foo[t](x: Int)
    |new foo[String](3) { () }
    |""")

  //new foo with bar
  @Test def testNewExpr7 = assertTypedTree(sm"""
    |{
    |  trait foo;
    |  trait bar;
    |  {
    |    final class $$anon extends foo with bar;
    |    new $$anon()
    |  }
    |}""")

  //new { anonymous }
  @Test def testNewExpr8 = assertTypedTree(sm"""
    |{
    |  final class $$anon {
    |    5
    |  };
    |  new $$anon()
    |}""")

  //new { val early = 1 } with Parent[Int] { body }
  @Test def testNewExpr9 = assertTypedTree(sm"""
    |{
    |  class Parent[t];
    |  {
    |    final class $$anon extends {
    |      val early = 1
    |    } with Parent[scala.Int] {
    |      "testNewExpr"
    |    };
    |    new $$anon()
    |  }
    |}""")

  //new Foo { self => }
  @Test def testNewExpr10 = assertTypedTree(sm"""
    |{
    |  class Foo;
    |  {
    |    final class $$anon extends Foo { self =>
    |      
    |    };
    |    new $$anon()
    |  }
    |}""")

  @Test def testReturn = assertTypedTree("def test: scala.Int = return 42")

  @Test def testFunc1 = assertTypedTree(
    "List(1, 2, 3).map((i: Int) => i - 1)")

  @Test def testFunc2 = assertTypedTree(
    "val sum: Seq[Int] => Int = _ reduceLeft (_+_)")

  @Test def testFunc3 = assertTypedTree(
    "List(1, 2, 3) map (_ - 1)")

  @Test def testFunc4 = assertTypedTree(
    "val x: String => Int = ((str: String) => 1)")

  @Test def testAssign1 = assertTypedTree("{ class F { var v = 1 }; val f = new F; (f.v = 5).toString }")

  @Test def testImport1 = assertTypedTree("{ import scala.collection.mutable }")

  @Test def testImport2 = assertTypedTree("{ import java.lang.{String=>Str} }")

  @Test def testImport3 = assertTypedTree("{ import java.lang.{String=>Str, Object=>_, _} }")

  @Test def testImport4 = assertTypedTree("{ import scala.collection._ }")
}

trait ClassTypedTests {
  @Test def testClass = assertTypedTree("class *")

  @Test def testClassWithBody = assertTypedTree(sm"""
    |class X {
    |  def y = "test"
    |}""")

  @Test def testClassWithPublicParams = assertTypedTree("class X(val x: scala.Int, val s: scala.Predef.String)")

  @Test def testClassWithParams1 = assertTypedTree("class X(x: scala.Int, s: scala.Predef.String)")

  @Test def testClassWithParams2 = assertTypedTree("class X(x: Int, s: String)")

  @Test def testClassWithParams3 = assertTypedTree("class X(implicit x: Int, s: String)")

  //TODO-REFLECT - Failed
  //Annotations processing is not implemented
  //@Test def testClassWithParams4 = assertTypedTree("class X(implicit @unchecked x: Int, s: String)")
  @Test def testClassWithParams4 = assertTypedTree("class X(implicit x: Int, s: String)")

  @Test def testClassWithParams5 = assertTypedTree(sm"""
    |{
    |  class Y {
    |    val x = 5
    |  };
    |  class X(override private[this] val x: scala.Int, s: scala.Predef.String) extends Y;
    |  ()
    |}""")

  @Test def testClassWithParams6 = assertTypedTree("class Y; class X(override private[this] val x: Int, s: String) extends Y")

  @Test def testClassWithParams7 = assertTypedTree("class X protected (val x: scala.Int, val s: scala.Predef.String)")

  @Test def testClassWithParams8 = assertTypedTree("class X(var x: scala.Int)")

  @Test def testClassWithParams9 = assertTypedTree("def test(x: scala.Int*) = 5")

  @Test def testClassWithByNameParam = assertTypedTree("class X(x: => scala.Int)")

  @Test def testClassWithDefault = assertTypedTree(sm"""
    |{
    |  class X(var x: scala.Int = 5);
    |  ()
    |}""")

  //TODO-REFLECT - Failed
//  expected:<...maccessor> protected[ def x: Int = X.this.x;
//    <accessor> <paramaccessor> protected] def x_=(x$1: Int): ...> 
//  but was:<...maccessor> protected[[zzz] def x: Int = X.this.x;
//    <accessor> <paramaccessor> protected[zzz]] def x_=(x$1: Int): ...>
//  @Test def testClassWithParams10 = assertTypedTree("class zzz { class X(protected[zzz] var x: Int) }")

  @Test def testClassWithParams11 = assertTypedTree(sm"""
    |{
    |  class F(x: scala.Int);
    |  trait E {
    |    var x: scala.Int
    |  };
    |  class X(override var x: scala.Int = 5) extends F(x) with E;
    |  ()
    |}""")

  @Test def testClassWithParams12 = assertTypedTree("class X(val y: scala.Int)()(var z: scala.Double)")

  @Test def testClassWithImplicitParams = assertTypedTree("class X(var i: scala.Int)(implicit val d: scala.Double, var f: scala.Float)")

  @Test def testClassWithEarly = assertTypedTree(sm"""
    |class X(var i: scala.Int) extends {
    |  val a = i;
    |  type B
    |} with scala.Serializable""")

  @Test def testClassWithThrow1 = assertTypedTree(sm"""
    |class Throw1 {
    |  throw new scala.`package`.Exception("exception!")
    |}""")  

  @Test def testClassWithThrow2 = assertTypedTree(sm"""
    |class Throw2 {
    |  var msg = "   ";
    |  val e = new scala.`package`.Exception(Throw2.this.msg);
    |  throw Throw2.this.e
    |}""")

  /*
    class Test {
      val (a, b) = (1, 2)
    }
  */
  @Test def testClassWithAssignmentWithTuple1 = assertTypedTree(sm"""
    |class Test {
    |  private[this] val x$$1 = (scala.Tuple2.apply(1, 2): @scala.unchecked) match {
    |    case scala.Tuple2((a @ _), (b @ _)) => scala.Tuple2.apply(a, b)
    |  };
    |  val a = Test.this.x$$1._1;
    |  val b = Test.this.x$$1._2
    |}""")

  @Test def testClassWithAssignmentWithTuple2 = assertTypedTree(
    code = sm"""
    |class Test {
    |  val (a, b) = (1).->(2)
    |}""")

  /*
    class Test {
      val List(one, three, five) = List(1,3,5)
    }
  */
  @Test def testClassWithPatternMatchInAssignment = assertTypedTree(sm"""
    |class Test {
    |  private[this] val x$$1 = (scala.collection.immutable.List.apply(1, 3, 5): @scala.unchecked) match {
    |    case scala.collection.immutable.List((one @ _), (three @ _), (five @ _)) => scala.Tuple3.apply(one, three, five)
    |  };
    |  val one = Test.this.x$$1._1;
    |  val three = Test.this.x$$1._2;
    |  val five = Test.this.x$$1._3
    |}""")

  //class A(l: List[_])
  @Test def testClassWithExistentialParameter1 = assertTypedTree(sm"""
    |class Test(l: (scala.`package`.List[_$$1] forSome { 
    |  type _$$1
    |}))""")

  @Test def testClassWithExistentialParameter2 = assertTypedTree(sm"""
    |class B(l: (scala.`package`.List[T] forSome { 
    |  type T
    |}))""")

  @Test def testClassWithCompoundTypeTree = assertTypedTree(sm"""
    |{
    |  trait A;
    |  trait B;
    |  abstract class C(val a: A with B) {
    |    def method(x: A with B with C {
    |      val x: scala.Float
    |    }): A with B
    |  };
    |  ()
    |}""")

  @Test def testClassWithSelectFromTypeTree = assertTypedTree(sm"""
    |{
    |  trait A {
    |    type T
    |  };
    |  class B(t: (A)#T);
    |  ()
    |}""")

  @Test def testImplicitClass = assertTypedTree(sm"""
    |{
    |  implicit class X(protected[this] var x: scala.Int);
    |  ()
    |}""")

  @Test def testAbstractClass = assertTypedTree("abstract class X(protected[this] var x: scala.Int)")

  @Test def testCaseClassWithParams1 = assertTypedTree(sm"""
    |{
    |  case class X(x: scala.Int, s: scala.Predef.String);
    |  ()
    |}""")

    //TODO-REFLECT - Failed
//     expected:<...  <synthetic> val X$[2: X = x$1.asInstanceOf[X];
//      X.this.x$1.==(X$2.x$1).&&(X.this.s.==(X$2.s)).&&(X$2].canEqual(X.this))
//     ...> 
//      but was:<...  <synthetic> val X$[1: X = x$1.asInstanceOf[X];
//      X.this.x$1.==(X$1.x$1).&&(X.this.s.==(X$1.s)).&&(X$1].canEqual(X.this))
//     ...>
//  @Test def testCaseClassWithParams2 = assertTypedTree(sm"""
//    |{
//    |  case class X(protected val x: scala.Int, s: scala.Predef.String);
//    |  ()
//    |}""")

    //TODO-REFLECT - Failure
//     expected:<...  <synthetic> val X$[3: X = x$1.asInstanceOf[X];
//      X.this.x.==(X$3.x).&&(X.this.s.==(X$3.s)).&&(X$3].canEqual(X.this))
//     ...> 
//     but was:<...  <synthetic> val X$[1: X = x$1.asInstanceOf[X];
//      X.this.x.==(X$1.x).&&(X.this.s.==(X$1.s)).&&(X$1].canEqual(X.this))
//     ...>
//  @Test def testCaseClassWithParams3 = assertTypedTree(sm"""
//    |{
//    |  case class X(implicit x: scala.Int, s: scala.Predef.String);
//    |  ()
//    |}""")

    //TODO-REFLECT - Failure
//     expected:<...  <synthetic> val X$[4: X = x$1.asInstanceOf[X];
//      X.this.x.==(X$4.x).&&(X.this.s.==(X$4.s)).&&(X$4].canEqual(X.this))
//      ...> 
//      but was:<...  <synthetic> val X$[1: X = x$1.asInstanceOf[X];
//      X.this.x.==(X$1.x).&&(X.this.s.==(X$1.s)).&&(X$1].canEqual(X.this))
//      ...>
//  @Test def testCaseClassWithParams4 = assertTypedTree(sm"""
//    |{
//    |  trait V {
//    |    val x: scala.Int
//    |  };
//    |  case class X(override val x: scala.Int, s: scala.Predef.String) extends scala.Cloneable;
//    |  ()
//    |}""")

  @Test def testCaseClassWithBody = assertTypedTree(sm"""
    |{
    |  case class X() {
    |    def y = "test"
    |  };
    |  ()
    |}""")

  @Test def testLocalClass = assertTypedTree(sm"""
    |def test = {
    |  class X(var a: scala.Int) {
    |    def y = "test"
    |  };
    |  new X(5)
    |}""")

    //TODO-REFLECT - Failed - the biggest problem
    /*
 expected:<def test: [X forSome { type X <: Product with Serializable{def a: Int; def a_=(x$1: Int): Unit; def y: String; def copy(a: Int): X; def copy$default$1: Int @scala.annotation.unchecked.uncheckedVariance} } = {
  case class X extends AnyRef with Product with Serializable {
    <caseaccessor> <paramaccessor> private[this] var a: Int = _;
    <caseaccessor> <accessor> <paramaccessor> def a: Int = X.this.a;
    <accessor> <paramaccessor> def a_=(x$1: Int): Unit = X.this.a = x$1;
    def <init>(a: Int): X = {
      X.super.<init>();
      ()
    };
    def y: String = "test";
    <synthetic> def copy(a: Int = a): X = new X(a);
    <synthetic> def copy$default$1: Int = X.this.a;
    override <synthetic> def productPrefix: String = "X";
    <synthetic> def productArity: Int = 1;
    <synthetic> def productElement(x$1: Int): Any = x$1 match {
      case 0 => X.this.a
      case _ => throw new IndexOutOfBoundsException(x$1.toString())
    };
    override <synthetic> def productIterator: Iterator[Any] = runtime.this.ScalaRunTime.typedProductIterator[Any](X.this);
    <synthetic> def canEqual(x$1: Any): Boolean = x$1.$isInstanceOf[X]();
    override <synthetic> def hashCode(): Int = {
      <synthetic> var acc: Int = -889275714;
      acc = Statics.this.mix(acc, a);
      Statics.this.finalizeHash(acc, 1)
    };
    override <synthetic> def toString(): String = ScalaRunTime.this._toString(X.this);
    override <synthetic> def equals(x$1: Any): Boolean = X.this.eq(x$1.asInstanceOf[Object]).||(x$1 match {
  case (_: X) => true
  case _ => false
}.&&({
      <synthetic> val X$2: X = x$1.asInstanceOf[X];
      X.this.a.==(X$2.a).&&(X$2.canEqual(X.this))
    }))
  };
  <synthetic> object X extends scala.runtime.AbstractFunction1[Int,X] with Serializable {
    def <init>(): X.type = {
      X.super.<init>();
      ()
    };
    final override <synthetic> def toString(): String = "X";
    case <synthetic> def apply(a: Int): X = new X(a);
    case <synthetic> def unapply(x$0: X): Option[Int] = if (x$0.==(null))
      scala.this.None
    else
      Some.apply[Int](x$0.a);
    <synthetic> private def readResolve(): Object = X]
  };
  new X(5)
}> but was:<def test: [Product with Serializable{def a: Int; def a_=(x$1: Int): Unit; def y: String} = {
  case class X extends AnyRef with Product with Serializable {
    <caseaccessor> <paramaccessor> private[this] var a: Int = _;
    <caseaccessor> <accessor> <paramaccessor> def a: Int = X.this.a;
    <accessor> <paramaccessor> def a_=(x$1: Int): Unit = X.this.a = x$1;
    def <init>(a: Int): X = {
      X.super.<init>();
      ()
    };
    def y: String = "test";
    override <synthetic> def productPrefix: String = "X";
    <synthetic> def productArity: Int = 1;
    <synthetic> def productElement(x$1: Int): Any = x$1 match {
      case 0 => X.this.a
      case _ => throw new IndexOutOfBoundsException(x$1.toString())
    };
    override <synthetic> def productIterator: Iterator[Any] = runtime.this.ScalaRunTime.typedProductIterator[Any](X.this);
    <synthetic> def canEqual(x$1: Any): Boolean = x$1.$isInstanceOf[X]();
    override <synthetic> def hashCode(): Int = {
      <synthetic> var acc: Int = -889275714;
      acc = Statics.this.mix(acc, a);
      Statics.this.finalizeHash(acc, 1)
    };
    override <synthetic> def toString(): String = ScalaRunTime.this._toString(X.this);
    override <synthetic> def equals(x$1: Any): Boolean = X.this.eq(x$1.asInstanceOf[Object]).||(x$1 match {
  case (_: X) => true
  case _ => false
}.&&({
      <synthetic> val X$1: X = x$1.asInstanceOf[X];
      X.this.a.==(X$1.a).&&(X$1.canEqual(X.this))
    }))]
  };
  new X(5)
}>
* 
*/
//  @Test def testLocalCaseClass = assertTypedTree(sm"""
//    |def test = {
//    |  case class X(var a: scala.Int) {
//    |    def y = "test"
//    |  };
//    |  new X(5)
//    |}""")

  @Test def testSuperInClass = assertTypedTree(sm"""
    |{
    |  trait Root {
    |    def r = "Root"
    |  };
    |  class X extends Root {
    |    def superX = super.r
    |  };
    |  class Y extends X with Root {
    |    class Inner {
    |      val myY = Y.super.r
    |    };
    |    def fromX = super[X].r;
    |    def fromRoot = super[Root].r
    |  };
    |  ()
    |}""")

  @Test def testThisInClass = assertTypedTree(sm"""
    |class Outer {
    |  class Inner {
    |    val outer = Outer.this
    |  };
    |  val self = this
    |}""")

    //TODO-REFLECT - Failure
//     expected:<...  <synthetic> val X$[6: X = x$1.asInstanceOf[X];
//      X.this.x.==(X$6.x).&&(X.this.s.==(X$6.s)).&&(X$6].canEqual(X.this))
// ...> but was:<...  <synthetic> val X$[1: X = x$1.asInstanceOf[X];
//      X.this.x.==(X$1.x).&&(X.this.s.==(X$1.s)).&&(X$1].canEqual(X.this))
// ...>
//  @Test def testCaseClassWithParamsAndBody = assertTypedTree(sm"""
//    |{
//    |  case class X(var x: scala.Int, var s: scala.Predef.String) {
//    |    def y = "test"
//    |  };
//    |  ()
//    |}""")

  @Test def testObject = assertTypedTree("object *")

  @Test def testObjectWithBody = assertTypedTree(sm"""
    |object X {
    |  def y = "test"
    |}""")

  @Test def testObjectWithEarly1 = assertTypedTree(sm"""
    |object X extends {
    |  val early: scala.Int = 42
    |} with scala.Serializable""")

  @Test def testObjectWithEarly2 = assertTypedTree(sm"""
    |object X extends {
    |  val early: scala.Int = 42;
    |  type EarlyT = scala.Predef.String
    |} with scala.Serializable""")

  @Test def testObjectWithSelf = assertTypedTree(sm"""
    |object Foo extends scala.Serializable { self =>
    |  42
    |}""")

  //TODO-REFLECT - Failure
//  expected:<...t = {
//    ()
//  };
//  []object X extends Any...> but was:<...t = {
//    ()
//  };
//  [private[Y] ]object X extends Any...>
//  @Test def testObjectInh = assertTypedTree(sm"""
//    |trait Y {
//    |  private[Y] object X extends scala.Serializable with scala.Cloneable
//    |}""")

  @Test def testObjectWithPatternMatch1 = assertTypedTree(sm"""
    |object PM1 {
    |  scala.collection.immutable.List.apply(1, 2) match {
    |    case (i @ _) => i
    |  }
    |}""")

  @Test def testObjectWithPatternMatch2 = assertTypedTree(
    sm"""
    |object PM2 {
    |  List(1, 2).map {
    |    case i if i > 5 => i
    |  }
    |}""")

    //TODO-REFLECT FAIL
//   expected:<...nt, List[Int]](((x0$[2: Int) => x0$2] match {
//    case (i...> but was:<...nt, List[Int]](((x0$[1: Int) => x0$1] match {
//    case (i...>
//  @Test def testObjectWithPatternMatch3 = assertTypedTree(
//    sm"""
//    |object PM3 {
//    |  List(1, 2).map {
//    |    case i: Int => i
//    |  }
//    |}""")

    //TODO-REFLECT FAIL
//   expected:<...nt, List[Int]](((x0$[3: Int) => x0$3] match {
//    case _ ...> but was:<...nt, List[Int]](((x0$[1: Int) => x0$1] match {
//    case _ ...>
//  @Test def testObjectWithPatternMatch4 = assertTypedTree(
//    sm"""
//    |object PM4 {
//    |  List(1, 2).map {
//    |    case _ => 42
//    |  }
//    |}""")

  @Test def testObjectWithPatternMatch5 = assertTypedTree(
    sm"""
    |object PM5 {
    |  List(1, 2) match {
    |    case x :: xs => x
    |  }
    |}""")

    //TODO-REFLECT FAIL
//   expected:<...List[Boolean]](((x0$[4: Int) => x0$4] match {
//    case (0...> but was:<...List[Boolean]](((x0$[1: Int) => x0$1] match {
//    case (0...>
//  @Test def testObjectWithPatternMatch6 = assertTypedTree(
//    sm"""
//    |object PM6 {
//    |  List(1, 2).map {
//    |    case (0 | 1) => true
//    |    case _ => false
//    |  }
//    |}""")

  @Test def testObjectWithPatternMatch7 = assertTypedTree(sm"""
    |object PM7 {
    |  scala.Predef.augmentString("abcde").toList match {
    |    case scala.collection.Seq((car @ _), _*) => car
    |  }
    |}""")

  @Test def testObjectWithPatternMatch8 = assertTypedTree(sm"""
    |{
    |  object Extractor {
    |    def unapply(i: scala.Int) = scala.Some.apply(i)
    |  };
    |  object PM9 {
    |    42 match {
    |      case (a @ Extractor((i @ _))) => i
    |    }
    |  };
    |  ()
    |}""")

  @Test def testObjectWithPartialFunc = assertTypedTree(sm"""
    |object Test {
    |  def partFuncTest[A, B](e: scala.`package`.Either[A, B]): scala.Unit = e match {
    |    case scala.`package`.Right(_) => ()
    |  }
    |}""")

  @Test def testObjectWithTry = assertTypedTree(
    sm"""
    |object Test {
    |  import java.io._;
    |  var file: PrintStream = null;
    |  try {
    |    val out = new FileOutputStream("myfile.txt");
    |    file = new PrintStream(out)
    |  } catch {
    |    case ioe: IOException => println("ioe")
    |    case e: Exception => println("e")
    |  } finally println("finally")
    |}""")
}

trait TraitTypedTests {
  @Test def testTrait = assertTypedTree("trait *")

  @Test def testTraitWithBody = assertTypedTree(sm"""
    |trait X {
    |  def y = "test"
    |}""")

  @Test def testTraitWithSelfTypeAndBody = assertTypedTree(sm"""
    |trait X { self: scala.Cloneable =>
    |  def y = "test"
    |}""")

  @Test def testTraitWithSelf1 = assertTypedTree(sm"""
    |trait X { self =>
    |  def y = "test"
    |}""")

  @Test def testTraitWithSelf2 = assertTypedTree(sm"""
    |trait X { self: scala.Cloneable with scala.Serializable =>
    |  val x: scala.Int = 1
    |}""")

  @Test def testTraitTypeParams = assertTypedTree("trait X[A, B]")

  @Test def testTraitWithBody2 = assertTypedTree(sm"""
    |trait X {
    |  def foo: scala.Unit;
    |  val bar: scala.Predef.String
    |}""")

  @Test def testTraitWithInh = assertTypedTree("trait X extends scala.Cloneable with scala.Serializable")

  @Test def testTraitWithEarly1 = assertTypedTree(sm"""
    |trait X extends {
    |  val x: Int = 1
    |} with AnyRef""")

  @Test def testTraitWithEarly2 = assertTypedTree(sm"""
    |trait X extends {
    |  val x: scala.Int = 0;
    |  type Foo = scala.Unit
    |} with scala.Cloneable""")

  @Test def testTraitWithEarly3 = assertTypedTree(sm"""
    |trait X extends {
    |  val x: scala.Int = 5;
    |  val y: scala.Double = 4.0;
    |  type Foo;
    |  type XString = scala.Predef.String
    |} with scala.Serializable""")

  @Test def testTraitWithEarly4 = assertTypedTree(sm"""
    |trait X extends {
    |  val x: scala.Int = 5;
    |  val y: scala.Double = 4.0;
    |  type Foo;
    |  type XString = scala.Predef.String
    |} with scala.Serializable {
    |  val z = 7
    |}""")

  @Test def testTraitWithSingletonTypeTree = assertTypedTree(sm"""
    |trait Test {
    |  def testReturnSingleton(): Test.this.type
    |}""")

  @Test def testTraitWithThis = assertTypedTree("{ trait X; trait Y; trait Test { this: X with Y => } }")

  @Test def testTraitWithWhile1 = assertTypedTree(sm"""
    |trait Test {
    |  while (false) 
    |    scala.Predef.println("testing...")
    |  
    |}""")

  @Test def testTraitWithWhile2 = assertTypedTree(sm"""
    |trait Test {
    |  while (true) 
    |    {
    |      scala.Predef.println("testing...");
    |      scala.Predef.println("testing...")
    |    }
    |  
    |}""")

  @Test def testTraitWithDoWhile1 = assertTypedTree(sm"""
    |trait Test {
    |  do 
    |    scala.Predef.println("testing...")
    |   while (true) 
    |}""")  

  @Test def testTraitWithTypes = assertTypedTree(
    sm"""
    |trait Test {
    |  type A = Int;
    |  type B >: Nothing <: AnyRef;
    |  protected type C >: Nothing;
    |  type D <: AnyRef
    |}""")
}

trait ValAndDefTypedTests {
  @Test def testVal1 = assertTypedTree("val a: scala.Unit = ()")

  @Test def testVal2 = assertTypedTree("val * : scala.Unit = ()")

  @Test def testVal3 = assertTypedTree("val a_ : scala.Unit = ()")

  @Test def testDef1 = assertTypedTree("def a = ()")

  @Test def testDef2 = assertTypedTree("def * : scala.Unit = ()")

  @Test def testDef3 = assertTypedTree("def a_(x: scala.Int): scala.Unit = ()")

  @Test def testDef4 = assertTypedTree("def a_ : scala.Unit = ()")

  @Test def testDef5 = assertTypedTree("def a_(* : scala.Int): scala.Unit = ()")

  @Test def testDef6 = assertTypedTree("def a_(b_ : scala.Int) = ()")

  @Test def testDef7 = assertTypedTree(sm"""
    |{
    |  def test1 = ();
    |  def test2() = ()
    |}""")

  @Test def testDef8 = assertTypedTree("def test[X, R[T]](implicit a: R[X]) = ()")

  @Test def testDef9 = assertTypedTree("def a(x: scala.Int)(implicit z: scala.Double, y: scala.Float): scala.Unit = ()")

  @Test def testDefWithLazyVal1 = assertTypedTree(
    code = "def a = { lazy val test: Int = 42 }")

  @Test def testDefWithLazyVal2 = assertTypedTree(sm"""
    |def a = {
    |  lazy val test = {
    |    scala.Predef.println();
    |    scala.Predef.println()
    |  };
    |  ()
    |}""")

  @Test def testDefWithParams1 = assertTypedTree("def foo(x: scala.Int*) = ()")

  @Test def testDefWithParams2 = assertTypedTree(sm"""
    |{
    |  def foo(x: scala.Int)(y: scala.Int = 1) = ();
    |  ()
    |}""")

  @Test def testDefWithTypeParams1 = assertTypedTree(sm"""
    |{
    |  def foo[A, B, C](x: A)(y: scala.Int = 1): C = ().asInstanceOf[C];
    |  ()
    |}""")

  @Test def testDefWithTypeParams2 = assertTypedTree("def foo[A, B <: scala.AnyVal] = ()")

  //TODO-REFLECT - Failure (Errors)
  //annotations can't be typed
//  @Test def testDefWithAnn1 = assertTypedTree("@annot def foo = null")
//
//  @Test def testDefWithAnn2 = assertTypedTree("@a(x) def foo = null")
//
//  @Test def testDefWithAnn3 = assertTypedTree("@Foo[A, B] def foo = null")
//
//  @Test def testDefWithAnn4 = assertTypedTree("@Foo(a)(b)(x, y) def foo = null")
//
//  @Test def testDefWithAnn5 = assertTypedTree("@Foo[A, B](a)(b) @Bar def foo(x: Int) = null")
//
//  @Test def testDefWithAnn6 = assertTypedTree("@test1(new test2()) def foo = 42")
//
//  @Test def testDefWithAnn7 = assertTypedTree("@`t*` def foo = 42")
//
//  @Test def testDefWithAnn8 = assertTypedTree("@throws(classOf[Exception]) def foo = throw new Exception()")
//
//  @Test def testAnnotated1 = assertTypedTree(
//    code = "def foo = 42: @baz")(
//    parsedCode = "def foo = 42: @baz",
//    typedCode = "def foo = (42: @baz)",
//    wrap = true)
//
//  @Test def testAnnotated2 = assertTypedTree(
//    code = "def foo = 42: @foo2[A1, B1](4)(2)")(
//    parsedCode = "def foo = 42: @foo2[A1, B1](4)(2)",
//    typedCode = "def foo = (42: @foo2[A1, B1](4)(2))",
//    wrap = true)
//
//  @Test def testAnnotated3 = assertTypedTree(
//    code = "def foo = (42: @foo1[A1, B1]): @foo2[A1, B1](4)(2)")(
//    parsedCode = "def foo = (42: @foo1[A1, B1]): @foo2[A1, B1](4)(2)",
//    typedCode = "def foo = ((42: @foo1[A1, B1]): @foo2[A1, B1](4)(2))",
//    wrap = true)
//
//  @Test def testAnnotated4 = assertTypedTree(
//    code = "def foo = 42: @foo3[A1, B1](4)(2.0F, new foo1[A1, B1]())")(
//    parsedCode = "def foo = 42: @foo3[A1, B1](4)(2.0F, new foo1[A1, B1]())",
//    typedCode = "def foo = (42: @foo3[A1, B1](4)(2.0F, new foo1[A1, B1]()))",
//    wrap = true)
//
//  @Test def testAnnotated5 = assertTypedTree(sm"""
//    |{
//    |  val x = 5;
//    |  (x: @unchecked) match {
//    |    case ((_): scala.Int) => true
//    |    case _ => false
//    |  }
//    |}""")
//
//  @Test def testAnnotated8 = assertTypedTree(sm"""
//    |{
//    |  val x = 5;
//    |  ((x: @unchecked): @foo3(4)(2.0F, new foo1[A1, B1]())) match {
//    |    case _ => true
//    |  }
//    |}""")
}

trait PackageTypedTests {
  //TODO-REFLECT - Failure
//  java.lang.AssertionError: assertion failed: <none>
//  at scala.Predef$.assert(Predef.scala:165)
//  at scala.reflect.internal.Symbols$Symbol.newPackage(Symbols.scala:309)
//
//  final def newPackage(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): ModuleSymbol = {
//    assert(name == nme.ROOT || isPackageClass, this)
//    newModule(name, pos, PackageFlags | newFlags)
//  }
//  @Test def testPackage1 = assertTypedTree(sm"""
//    |package foo.bar {
//    |  
//    |}""")
//
//  @Test def testPackage2 = assertTypedTree(sm"""
//    |package foo {
//    |  class C
//    |
//    |  object D
//    |}""")
//
//  //package object foo extends a with b
//  @Test def testPackage3 = assertTypedTree(sm"""
//    |package foo {
//    |  object `package` extends a with b
//    |}""")
//
//  //package object foo { def foo; val x = 1 }
//  @Test def testPackage4 = assertTypedTree(sm"""
//    |package foo {
//    |  object `package` {
//    |    def foo: scala.Unit = ();
//    |    val x = 1
//    |  }
//    |}""")
//
//  //package object foo extends { val x = 1; type I = Int } with Any
//  @Test def testPackage5 = assertTypedTree(sm"""
//    |package foo {
//    |  object `package` extends {
//    |    val x = 1;
//    |    type I = Int
//    |  } with AnyRef
//    |}""")
}
