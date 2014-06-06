/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

// Added: Sat Oct 7 16:08:21 2006
//todo: use inherited type info also for vars and values

// Added: Thu Apr 12 18:23:58 2007
//todo: disallow C#D in superclass
//todo: treat :::= correctly
package scala
package tools.nsc
package typechecker

import scala.reflect.internal.util.Statistics

trait Typers extends scala.reflect.internal.tools.nsc.typechecker.Typers with Adaptations with Tags with TypersTracking with PatternTypers {
  self: Analyzer =>
    
//    abstract class Typer(context0: Context) extends TyperDiagnostics with Adaptation with Tag with PatternTyper with TyperContextErrors
}

//TODO-REFLECT moved to reflect
//object TypersStats {
//  import scala.reflect.internal.TypesStats._
//  val typedIdentCount     = Statistics.newCounter("#typechecked identifiers")
//  val typedSelectCount    = Statistics.newCounter("#typechecked selections")
//  val typedApplyCount     = Statistics.newCounter("#typechecked applications")
//  val rawTypeFailed       = Statistics.newSubCounter ("  of which in failed", rawTypeCount)
//  val subtypeFailed       = Statistics.newSubCounter("  of which in failed", subtypeCount)
//  val findMemberFailed    = Statistics.newSubCounter("  of which in failed", findMemberCount)
//  val failedSilentNanos   = Statistics.newSubTimer("time spent in failed", typerNanos)
//  val failedApplyNanos    = Statistics.newSubTimer("  failed apply", typerNanos)
//  val failedOpEqNanos     = Statistics.newSubTimer("  failed op=", typerNanos)
//  val isReferencedNanos   = Statistics.newSubTimer("time spent ref scanning", typerNanos)
//  val visitsByType        = Statistics.newByClass("#visits by tree node", "typer")(Statistics.newCounter(""))
//  val byTypeNanos         = Statistics.newByClass("time spent by tree node", "typer")(Statistics.newStackableTimer("", typerNanos))
//  val byTypeStack         = Statistics.newTimerStack()
//}
