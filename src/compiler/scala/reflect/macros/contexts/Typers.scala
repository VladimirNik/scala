package scala.reflect.macros
package contexts

import scala.reflect.moved.macros.contexts.{ Typers => RTypers }

trait Typers extends RTypers {
  self: Context =>
}
