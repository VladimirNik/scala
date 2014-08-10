package scala.reflect.macros
package runtime

import scala.reflect.moved.macros.runtime.{ JavaReflectionRuntimes => RJavaReflectionRuntimes }

trait JavaReflectionRuntimes extends RJavaReflectionRuntimes {
  self: scala.tools.nsc.typechecker.Analyzer =>
}