package scala.tools.nsc.transform.patmat

import scala.reflect.internal.tools.nsc.transform.patmat.{ ScalacPatternExpanders => RScalacPatternExpanders }
import scala.tools.nsc.Global

trait ScalacPatternExpanders extends RScalacPatternExpanders {
  val global: Global
}