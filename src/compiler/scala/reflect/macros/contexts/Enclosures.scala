package scala.reflect.macros
package contexts

import scala.reflect.{ClassTag, classTag}
import scala.reflect.moved.macros.contexts.{ Enclosures => REnclosures }

trait Enclosures extends REnclosures {
  self: Context =>
}
