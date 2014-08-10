package scala.reflect.reify

import scala.reflect.macros.contexts.Context

abstract class Taggers extends scala.reflect.moved.reify.Taggers {
  val c: Context
}
