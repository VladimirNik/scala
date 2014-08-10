/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package typechecker

import scala.reflect.internal.tools.nsc.typechecker.{ ConstantFolder => RConstantFolder }

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class ConstantFolder extends RConstantFolder {
  val global: Global
}
