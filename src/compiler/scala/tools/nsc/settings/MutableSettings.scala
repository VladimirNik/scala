package scala.tools.nsc.settings

import scala.reflect.internal.tools.nsc.settings.{ MutableSettings => RMutableSettings }

class MutableSettings(override val errorFn: String => Unit) extends RMutableSettings(errorFn)