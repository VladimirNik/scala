package scala.reflect.macros
package runtime

import scala.reflect.internal.util.Position
import scala.util.control.ControlThrowable
import scala.reflect.moved.macros.runtime.{ AbortMacroException => RAbortMacroException }

class AbortMacroException(override val pos: Position, override val msg: String) extends RAbortMacroException(pos, msg)