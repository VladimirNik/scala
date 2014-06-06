package scala.tools.reflect

import scala.reflect.macros.runtime.Context

abstract class FormatInterpolator extends scala.reflect.internal.tools.reflect.FormatInterpolator {
  val c: Context
  override val global: c.universe.type = c.universe
  override val stdContextTags = new { val tc: c.type = c } with StdContextTags
}
