/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

/** This trait provides logic for assessing the validity of argument
 *  adaptations, such as tupling, unit-insertion, widening, etc.  Such
 *  logic is spread around the compiler, without much ability on the
 *  part of the user to tighten the potentially dangerous bits.
 *
 *  TODO: unifying/consolidating said logic under consistent management.
 *
 *  @author  Paul Phillips
 */
trait Adaptations extends scala.reflect.internal.tools.nsc.typechecker.Adaptations {
  self: Analyzer =>
}
