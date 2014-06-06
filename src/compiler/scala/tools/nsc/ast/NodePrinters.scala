/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

/** The object `nodePrinter` converts the internal tree
 *  representation to a string.
 *
 *  @author  Stephane Micheloud
 *  @author  Paul Phillips
 */
abstract class NodePrinters extends scala.reflect.internal.tools.nsc.ast.NodePrinters {
  val global: Global
}
