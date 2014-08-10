package scala.reflect.macros
package compiler

import scala.tools.nsc.Global
import scala.reflect.moved.macros.compiler.{ DefaultMacroCompiler => RDefaultMacroCompiler }

abstract class DefaultMacroCompiler extends RDefaultMacroCompiler
                                       with Resolvers
                                       with Validators
                                       with Errors {
  val global: Global
}