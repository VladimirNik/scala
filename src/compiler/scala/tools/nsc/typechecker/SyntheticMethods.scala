/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package typechecker

trait SyntheticMethods extends scala.reflect.internal.tools.nsc.typechecker.SyntheticMethods with ast.TreeDSL {
  self: Analyzer =>
}
