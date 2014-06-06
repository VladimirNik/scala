package scala.reflect.macros
package compiler

import scala.reflect.internal.Flags._
import scala.reflect.moved.macros.compiler.{ Validators => RValidators }

trait Validators extends RValidators {
  self: DefaultMacroCompiler =>
}
