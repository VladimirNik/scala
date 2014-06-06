/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.reflect.macros
package contexts

import scala.reflect.moved.macros.contexts.{ Reifiers => RReifiers }

trait Reifiers extends RReifiers {
  self: Context =>
}
