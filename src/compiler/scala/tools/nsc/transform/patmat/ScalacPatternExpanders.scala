/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc.transform.patmat

import scala.reflect.internal.tools.nsc.transform.patmat.{ ScalacPatternExpanders => RScalacPatternExpanders }
import scala.tools.nsc.Global

trait ScalacPatternExpanders extends RScalacPatternExpanders {
  val global: Global
}
