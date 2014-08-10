package scala.tools.nsc
package typechecker

import scala.reflect.macros.util._
import scala.reflect.macros.runtime.MacroRuntimes

trait Macros extends scala.reflect.internal.tools.nsc.typechecker.Macros with MacroRuntimes with Traces with Helpers {
  self: Analyzer =>
}

//TODO-REFLECT moved to reflect
//object MacrosStats {
//  import scala.reflect.internal.TypesStats.typerNanos
//  val macroExpandCount    = Statistics.newCounter ("#macro expansions", "typer")
//  val macroExpandNanos    = Statistics.newSubTimer("time spent in macroExpand", typerNanos)
//}
//
//class Fingerprint private[Fingerprint](val value: Int) extends AnyVal {
//  def paramPos = { assert(isTag, this); value }
//  def isTag = value >= 0
//  override def toString = this match {
//    case Other => "Other"
//    case LiftedTyped => "Expr"
//    case LiftedUntyped => "Tree"
//    case _ => s"Tag($value)"
//  }
//}
//
//object Fingerprint {
//  def apply(value: Int) = new Fingerprint(value)
//  def Tagged(tparamPos: Int) = new Fingerprint(tparamPos)
//  val Other = new Fingerprint(-1)
//  val LiftedTyped = new Fingerprint(-2)
//  val LiftedUntyped = new Fingerprint(-3)
//}
