package scala.reflect.reify
package phases

import scala.reflect.reify.codegen._

trait Reify extends scala.reflect.moved.reify.phases.Reify
               with GenSymbols
               with GenTypes
               with GenNames
               with GenTrees
               with GenAnnotationInfos
               with GenPositions
               with GenUtils {

  self: Reifier =>
}
