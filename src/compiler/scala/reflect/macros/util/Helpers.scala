package scala.reflect.macros
package util

import scala.tools.nsc.typechecker.Analyzer
import scala.reflect.moved.macros.util.{ Helpers => RHelpers }

trait Helpers extends RHelpers {
  self: Analyzer =>
}
