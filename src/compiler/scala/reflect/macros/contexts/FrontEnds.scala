package scala.reflect.macros
package contexts

import scala.reflect.moved.macros.contexts.{ FrontEnds => RFrontEnds }

trait FrontEnds extends RFrontEnds {
  self: Context =>
}
