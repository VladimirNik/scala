/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 *
 * @author  Paul Phillips
 */

package scala.tools.nsc
package ast

import scala.reflect.internal.tools.nsc.ast.{TreeDSL => TreeDSLBase}

/** A DSL for generating scala code.  The goal is that the
 *  code generating code should look a lot like the code it
 *  generates.
 */

trait TreeDSL extends TreeDSLBase {
  val global: Global
}
