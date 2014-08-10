package scala.tools
package reflect

import scala.reflect.{ClassTag, classTag}
import scala.reflect.api.{Mirror, TypeCreator, Universe => ApiUniverse}
import scala.reflect.internal.tools.reflect.{ StdContextTags => RStdContextTags }

// [Eugene++] Before 2.10 is released, I suggest we don't rely on automated type tag generation
// sure, it's convenient, but then refactoring reflection / reification becomes a pain
// `ClassTag` tags are fine, because they don't need a reifier to be generated

trait StdTags extends scala.reflect.internal.tools.reflect.StdTags {
  val u: ApiUniverse with Singleton
  val m: Mirror[u.type]
}

object StdRuntimeTags extends StdTags {
  val u: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  val m = u.runtimeMirror(getClass.getClassLoader)
  // we need getClass.getClassLoader to support the stuff from scala-compiler.jar
}

//TODO-REFLECT try to remove u and m here
abstract class StdContextTags extends RStdContextTags {
  val tc: scala.reflect.macros.contexts.Context
  override val u: tc.universe.type = tc.universe
  override val m = tc.mirror
}
