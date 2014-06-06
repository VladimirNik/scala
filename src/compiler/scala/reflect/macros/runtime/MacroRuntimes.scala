package scala.reflect.macros
package runtime

import scala.reflect.moved.macros.runtime.{ MacroRuntimes => RMacroRuntimes }

trait MacroRuntimes extends RMacroRuntimes with JavaReflectionRuntimes {
  self: scala.tools.nsc.typechecker.Analyzer =>
}