/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ immutable, mutable }
import scala.annotation.tailrec
import scala.reflect.internal.util.shortClassOfInstance

/**
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Contexts extends scala.reflect.internal.tools.nsc.typechecker.Contexts { 
  self: Analyzer =>
  import global._
  
//  class Context(val tree: Tree, val owner: Symbol, val scope: Scope,
//    val unit: CompilationUnit, _outer: Context) extends super.Context(tree, owner, scope, unit, _outer)
}

//TODO-REFLECT ContextMode is moved to reflect
//object ContextMode {
//  import scala.language.implicitConversions
//  private implicit def liftIntBitsToContextState(bits: Int): ContextMode = apply(bits)
//  def apply(bits: Int): ContextMode = new ContextMode(bits)
//  final val NOmode: ContextMode                   = 0
//
//  final val ReportErrors: ContextMode             = 1 << 0
//  final val BufferErrors: ContextMode             = 1 << 1
//  final val AmbiguousErrors: ContextMode          = 1 << 2
//
//  /** Are we in a secondary constructor after the this constructor call? */
//  final val ConstructorSuffix: ContextMode        = 1 << 3
//
//  /** For method context: were returns encountered? */
//  final val ReturnsSeen: ContextMode              = 1 << 4
//
//  /** Is this context (enclosed in) a constructor call?
//    * (the call to the super or self constructor in the first line of a constructor.)
//    * In such a context, the object's fields should not be in scope
//    */
//  final val SelfSuperCall: ContextMode            = 1 << 5
//
//  // TODO harvest documentation for this
//  final val ImplicitsEnabled: ContextMode         = 1 << 6
//
//  final val MacrosEnabled: ContextMode            = 1 << 7
//
//  /** To selectively allow enrichment in patterns, where other kinds of implicit conversions are not allowed */
//  final val EnrichmentEnabled: ContextMode        = 1 << 8
//
//  /** Are we in a run of [[scala.tools.nsc.typechecker.TreeCheckers]]? */
//  final val Checking: ContextMode                 = 1 << 9
//
//  /** Are we retypechecking arguments independently from the function applied to them? See `Typer.tryTypedApply`
//   *  TODO - iron out distinction/overlap with SecondTry.
//   */
//  final val ReTyping: ContextMode                 = 1 << 10
//
//  /** Are we typechecking pattern alternatives. Formerly ALTmode. */
//  final val PatternAlternative: ContextMode       = 1 << 11
//
//  /** Are star patterns allowed. Formerly STARmode. */
//  final val StarPatterns: ContextMode             = 1 << 12
//
//  /** Are we typing the "super" in a superclass constructor call super.<init>. Formerly SUPERCONSTRmode. */
//  final val SuperInit: ContextMode                = 1 << 13
//
//  /*  Is this the second attempt to type this tree? In that case functions
//   *  may no longer be coerced with implicit views. Formerly SNDTRYmode.
//   */
//  final val SecondTry: ContextMode                = 1 << 14
//
//  /** Are we in return position? Formerly RETmode. */
//  final val ReturnExpr: ContextMode               = 1 << 15
//
//  /** Are unapplied type constructors allowed here? Formerly HKmode. */
//  final val TypeConstructorAllowed: ContextMode   = 1 << 16
//
//  /** TODO: The "sticky modes" are EXPRmode, PATTERNmode, TYPEmode.
//   *  To mimick the sticky mode behavior, when captain stickyfingers
//   *  comes around we need to propagate those modes but forget the other
//   *  context modes which were once mode bits; those being so far the
//   *  ones listed here.
//   */
//  final val FormerNonStickyModes: ContextMode = (
//    PatternAlternative | StarPatterns | SuperInit | SecondTry | ReturnExpr | TypeConstructorAllowed
//  )
//
//  final val DefaultMode: ContextMode      = MacrosEnabled
//
//  private val contextModeNameMap = Map(
//    ReportErrors           -> "ReportErrors",
//    BufferErrors           -> "BufferErrors",
//    AmbiguousErrors        -> "AmbiguousErrors",
//    ConstructorSuffix      -> "ConstructorSuffix",
//    SelfSuperCall          -> "SelfSuperCall",
//    ImplicitsEnabled       -> "ImplicitsEnabled",
//    MacrosEnabled          -> "MacrosEnabled",
//    Checking               -> "Checking",
//    ReTyping               -> "ReTyping",
//    PatternAlternative     -> "PatternAlternative",
//    StarPatterns           -> "StarPatterns",
//    SuperInit              -> "SuperInit",
//    SecondTry              -> "SecondTry",
//    TypeConstructorAllowed -> "TypeConstructorAllowed"
//  )
//}
//
///**
// * A value class to carry the boolean flags of a context, such as whether errors should
// * be buffered or reported.
// */
//final class ContextMode private (val bits: Int) extends AnyVal {
//  import ContextMode._
//
//  def &(other: ContextMode): ContextMode  = new ContextMode(bits & other.bits)
//  def |(other: ContextMode): ContextMode  = new ContextMode(bits | other.bits)
//  def &~(other: ContextMode): ContextMode = new ContextMode(bits & ~(other.bits))
//  def set(value: Boolean, mask: ContextMode) = if (value) |(mask) else &~(mask)
//
//  def inAll(required: ContextMode)        = (this & required) == required
//  def inAny(required: ContextMode)        = (this & required) != NOmode
//  def inNone(prohibited: ContextMode)     = (this & prohibited) == NOmode
//
//  override def toString =
//    if (bits == 0) "NOmode"
//    else (contextModeNameMap filterKeys inAll).values.toList.sorted mkString " "
//}
