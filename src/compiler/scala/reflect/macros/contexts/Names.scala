package scala.reflect.macros
package contexts

import scala.reflect.moved.macros.contexts.{ Names => RNames }

trait Names extends RNames {
  self: Context =>
}