package scala.reflect.macros
package contexts

import scala.reflect.moved.macros.contexts.{ Aliases => RAliases }

trait Aliases extends RAliases {
  self: Context =>
}