package scala.reflect.moved.macros.runtime

import scala.reflect.internal.util.Position
import scala.util.control.ControlThrowable

class AbortMacroException(val pos: Position, val msg: String) extends Throwable(msg) with ControlThrowable