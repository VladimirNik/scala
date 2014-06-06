package scala.reflect.macros
package contexts

import scala.reflect.moved.macros.contexts.{ ExprUtils => RExprUtils }

trait ExprUtils extends RExprUtils {
  self: Context =>
}
