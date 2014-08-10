package scala.reflect.macros
package compiler

import scala.reflect.macros.util.Traces
import scala.reflect.moved.macros.compiler.{ Errors => RErrors }

trait Errors extends RErrors with Traces {
  self: DefaultMacroCompiler =>
}
