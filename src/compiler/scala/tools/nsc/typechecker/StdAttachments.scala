package scala.tools.nsc
package typechecker

import scala.reflect.internal.tools.nsc.typechecker.{ StdAttachments => RStdAttachments }

trait StdAttachments extends RStdAttachments {
  self: Analyzer =>
}