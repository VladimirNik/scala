/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

trait Unapplies extends scala.reflect.internal.tools.nsc.typechecker.Unapplies with ast.TreeDSL {
  self: Analyzer =>
}
