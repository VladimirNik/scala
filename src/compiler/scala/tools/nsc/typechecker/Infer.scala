/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import scala.util.control.ControlThrowable
import symtab.Flags._
import scala.reflect.internal.Depth

/** This trait contains methods related to type parameter inference.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
trait Infer extends scala.reflect.internal.tools.nsc.typechecker.Infer with Checkable {
  self: Analyzer =>
}
