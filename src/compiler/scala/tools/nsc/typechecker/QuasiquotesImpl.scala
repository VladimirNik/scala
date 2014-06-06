package scala.tools.nsc.typechecker

import scala.tools.nsc.Global
import scala.tools.reflect.quasiquotes.Quasiquotes

//TODO-REFLECT - move to typechecker package and analyzer cake?
trait QuasiquotesImpl {
  self: Global =>
  override def context2quasiquoteImpl(c0: analyzer.MacroContext): Quasiquotes { val c: c0.type } =
    new { val c: c0.type = c0 } with Quasiquotes
}