package scala.reflect.macros
package compiler

import scala.reflect.moved.macros.compiler.{ Resolvers => RResolvers }

trait Resolvers extends RResolvers {
  self: DefaultMacroCompiler =>
}
