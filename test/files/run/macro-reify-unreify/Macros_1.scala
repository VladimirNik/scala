import scala.reflect.macros.blackbox.Context

object Macros {
  def foo(s: String) = macro Impls.foo

  object Impls {
    def foo(c: Context)(s: c.Expr[String]) = {
      import c.universe._
      import treeBuild._

      val world = c.reifyTree(mkRuntimeUniverseRef, EmptyTree, s.tree)
      val greeting = c.reifyTree(mkRuntimeUniverseRef, EmptyTree, c.typecheck(Apply(Select(Literal(Constant("hello ")), TermName("$plus")), List(c.unreifyTree(world)))))
      val typedGreeting = c.Expr[String](greeting)

      c.universe.reify {
        println("hello " + s.splice + " = " + typedGreeting.splice)
      }
    }
  }
}