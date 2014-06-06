package scala.reflect.reify

import scala.tools.nsc.Global
import scala.reflect.macros.ReificationException
import scala.reflect.macros.UnexpectedReificationException
import scala.reflect.reify.utils.Utils

/** Given a tree or a type, generate a tree that when executed at runtime produces the original tree or type.
 *  See more info in the comments to `reify` in scala.reflect.api.Universe.
 *
 *  @author   Martin Odersky
 *  @version  2.10
 *  @since    2.10
 */
abstract class Reifier extends scala.reflect.moved.reify.Reifier
                          with States
                          with Phases
                          with Errors
                          with Utils {

  val global: Global
}
