package scala.reflect.reify

import phases._

trait Phases extends scala.reflect.moved.reify.Phases
                with Reshape
                with Calculate
                with Metalevels
                with Reify {

  self: Reifier =>
}
