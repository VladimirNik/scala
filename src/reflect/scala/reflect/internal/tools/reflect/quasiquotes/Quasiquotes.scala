package scala.reflect.internal.tools.reflect
package quasiquotes

import scala.reflect.moved.macros.runtime.Context

abstract class Quasiquotes {
  val c: Context
  val global: c.universe.type = c.universe
  import c.universe._

  def expandQuasiquote: Tree
}