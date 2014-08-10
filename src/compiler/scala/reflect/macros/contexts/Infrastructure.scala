package scala.reflect.macros
package contexts

import scala.reflect.moved.macros.contexts.{ Infrastructure => RInfrastructure }

trait Infrastructure extends RInfrastructure {
  self: Context =>
}
