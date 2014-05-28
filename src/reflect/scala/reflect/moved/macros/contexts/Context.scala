package scala.reflect.moved.macros.contexts

import scala.reflect.internal.tools.nsc.ReflectGlobal

abstract class Context extends scala.reflect.macros.blackbox.Context
                          with scala.reflect.macros.whitebox.Context
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

  val universe: ReflectGlobal

  val mirror: universe.Mirror = universe.rootMirror

  val callsiteTyper: universe.analyzer.Typer

  val prefix: Expr[PrefixType]

  val expandee: Tree
}
