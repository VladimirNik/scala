/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

trait Checkable extends scala.reflect.internal.tools.nsc.typechecker.Checkable {
  self: Analyzer =>
}

//private[typechecker] final class Checkability(val value: Int) extends AnyVal { }
//private[typechecker] object Checkability {
//  val StaticallyTrue    = new Checkability(0)
//  val StaticallyFalse   = new Checkability(1)
//  val RuntimeCheckable  = new Checkability(2)
//  val Uncheckable       = new Checkability(3)
//  val CheckabilityError = new Checkability(4)
//}
