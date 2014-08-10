package scala.reflect.macros
package contexts

import scala.tools.nsc.Global

abstract class Context extends scala.reflect.moved.macros.contexts.Context
                          with Aliases
                          with Enclosures
                          with Names
                          with Reifiers
                          with FrontEnds
                          with Infrastructure
                          with Typers
                          with Parsers
                          with Evals
                          with ExprUtils
                          with Traces
                          with Internals {

  val universe: Global

  override val mirror: universe.Mirror = universe.rootMirror

  val callsiteTyper: universe.analyzer.Typer

  val prefix: Expr[PrefixType]

  val expandee: Tree
  
  //TODO-REFLECT refactor (there are 2 implementations in parents
//  override def eval[T](expr: Expr[T]): T = super[Evals].eval(expr)
//  override lazy val internal: ContextInternalApi = super[Internals].internal
//  override def parse(code: String): Tree = super[Parsers].parse(code) 
}
