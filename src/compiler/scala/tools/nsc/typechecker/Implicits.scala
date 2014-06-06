/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

//todo: rewrite or disllow new T where T is a mixin (currently: <init> not a member of T)
//todo: use inherited type info also for vars and values
//todo: disallow C#D in superclass
//todo: treat :::= correctly

package scala
package tools.nsc
package typechecker

import scala.annotation.tailrec
import scala.collection.{ mutable, immutable }
import mutable.{ LinkedHashMap, ListBuffer }
import scala.util.matching.Regex
import symtab.Flags._
import scala.reflect.internal.util.{TriState, Statistics}
import scala.language.implicitConversions

/** This trait provides methods to find various kinds of implicits.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Implicits extends scala.reflect.internal.tools.nsc.typechecker.Implicits {
  self: Analyzer =>
}

//TODO-REFLECT moved to reflect
//object ImplicitsStats {
//
//  import scala.reflect.internal.TypesStats._
//
//  val rawTypeImpl         = Statistics.newSubCounter ("  of which in implicits", rawTypeCount)
//  val subtypeImpl         = Statistics.newSubCounter("  of which in implicit", subtypeCount)
//  val findMemberImpl      = Statistics.newSubCounter("  of which in implicit", findMemberCount)
//  val subtypeAppInfos     = Statistics.newSubCounter("  of which in app impl", subtypeCount)
//  val implicitSearchCount = Statistics.newCounter   ("#implicit searches", "typer")
//  val plausiblyCompatibleImplicits
//                                  = Statistics.newSubCounter("  #plausibly compatible", implicitSearchCount)
//  val matchingImplicits   = Statistics.newSubCounter("  #matching", implicitSearchCount)
//  val typedImplicits      = Statistics.newSubCounter("  #typed", implicitSearchCount)
//  val foundImplicits      = Statistics.newSubCounter("  #found", implicitSearchCount)
//  val improvesCount       = Statistics.newSubCounter("implicit improves tests", implicitSearchCount)
//  val improvesCachedCount = Statistics.newSubCounter("#implicit improves cached ", implicitSearchCount)
//  val inscopeImplicitHits = Statistics.newSubCounter("#implicit inscope hits", implicitSearchCount)
//  val oftypeImplicitHits  = Statistics.newSubCounter("#implicit oftype hits ", implicitSearchCount)
//  val implicitNanos       = Statistics.newSubTimer  ("time spent in implicits", typerNanos)
//  val inscopeSucceedNanos = Statistics.newSubTimer  ("  successful in scope", typerNanos)
//  val inscopeFailNanos    = Statistics.newSubTimer  ("  failed in scope", typerNanos)
//  val oftypeSucceedNanos  = Statistics.newSubTimer  ("  successful of type", typerNanos)
//  val oftypeFailNanos     = Statistics.newSubTimer  ("  failed of type", typerNanos)
//  val subtypeETNanos      = Statistics.newSubTimer  ("  assembling parts", typerNanos)
//  val matchesPtNanos      = Statistics.newSubTimer  ("  matchesPT", typerNanos)
//  val implicitCacheAccs   = Statistics.newCounter   ("implicit cache accesses", "typer")
//  val implicitCacheHits   = Statistics.newSubCounter("implicit cache hits", implicitCacheAccs)
//}
