package scala.reflect.macros
package contexts

import scala.reflect.moved.macros.contexts.{ Traces => RTraces }

trait Traces extends RTraces {
  self: Context =>
}
