package scala.reflect.internal

import org.junit.Test
import org.junit.Assert._
import scala.tools.reflect._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ReflectGlobalTest extends BaseTypedTests

object ReflectGlobalHelper {
  val toolbox = cm.mkToolBox()

  import scala.reflect.internal.Chars._
  private def normalizeEOL(resultCode: String) =
    resultCode.lines mkString s"$LF"

  def assertTypedTree(code: String, wrap: Boolean = false, printRoot: Boolean = false) = {
    def toolboxTree(tree: => Tree) = try{
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

//    val parsedTree = toolboxTree(toolbox.parse(wrapCode(code)))
//    val typedToolboxTree = toolboxTree(toolbox.typecheck(parsedTree))
//    val typecheckedTree = typecheckTree(parsedTree)
//
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
    val typecheckedTree = typecheckTree(parsedTree)
    assertEquals("using toolbox and typecheckTree" + LF, show(typedToolboxTree), normalizeEOL(show(typecheckedTree)))
  }

//  def assertTreeCode(tree: Tree)(code: String) = {
//    assertEquals("using quasiquote or given tree"+LF, code.trim, normalizeEOL(showCode(tree)))
//  }

//  def assertPrintedCode(source: String, checkTypedTree: Boolean = true, wrapCode: Boolean = false) = {
//    if (checkTypedTree)
//      assertResultCode(source)(source, source, wrapCode)
//    else assertResultCode(source)(parsedCode = source, wrap = wrapCode)
//  }

  implicit class StrContextStripMarginOps(val stringContext: StringContext) extends util.StripMarginInterpolator
}

import ReflectGlobalHelper._

trait BaseTypedTests {
  @Test def testName1 = assertTypedTree("class X")
}