package scala.reflect.internal.tools.nsc
package transform.patmat

trait TreeAndTypeAnalysis {
  val global: TypecheckerRequirements

  import global._

  //TODO-REFLECT this code is removed from MatchAnalysis
    /** Compute the type T implied for a value `v` matched by a pattern `pat` (with expected type `pt`).
   *
   * Usually, this is the pattern's type because pattern matching implies instance-of checks.
   *
   * However, Stable Identifier and Literal patterns are matched using `==`,
   * which does not imply a type for the binder that binds the matched value.
   *
   * See SI-1503, SI-5024: don't cast binders to types we're not sure they have
   *
   * TODO: update spec as follows (deviation between `**`):
   *
   *   A pattern binder x@p consists of a pattern variable x and a pattern p.
   *   The type of the variable x is the static type T **IMPLIED BY** the pattern p.
   *   This pattern matches any value v matched by the pattern p
   *     **Deleted: , provided the run-time type of v is also an instance of T, **
   *   and it binds the variable name to that value.
   *
   *   Addition:
   *     A pattern `p` _implies_ a type `T` if the pattern matches only values of the type `T`.
   */
  def binderTypeImpliedByPattern(pat: Tree, pt: Type, binder: Symbol): Type =
    pat match {
      // because `==` decides whether these patterns match, stable identifier patterns (ident or selection)
      // do not contribute any type information (beyond the pattern's expected type)
      // e.g., in case x@Nil => x --> all we know about `x` is that it satisfies Nil == x, which could be anything
      case Ident(_) | Select(_, _) =>
        if (settings.future) pt
        else {
          // TODO: don't warn unless this unsound assumption is actually used in a cast
          // I tried annotating the type returned here with an internal annotation (`pat.tpe withAnnotation UnsoundAssumptionAnnotation`),
          // and catching it in the patmat backend when used in a cast (because that would signal the unsound assumption was used),
          // but the annotation didn't bubble up...
          // This is a pretty poor approximation.
          def unsoundAssumptionUsed = binder.name != nme.WILDCARD && !(pt <:< pat.tpe)
          if (settings.lint && unsoundAssumptionUsed)
            global.currentUnit.warning(pat.pos,
              sm"""The value matched by $pat is bound to ${binder.name}, which may be used under the
                  |unsound assumption that it has type ${pat.tpe}, whereas we can only safely
                  |count on it having type $pt, as the pattern is matched using `==` (see SI-1503).""")

          pat.tpe
        }


      // the other patterns imply type tests, so we can safely assume the binder has the pattern's type when the pattern matches
      // concretely, a literal, type pattern, a case class (the constructor's result type) or extractor (the unapply's argument type) all imply type tests
      // (and, inductively, an alternative)
      case _ => pat.tpe
    }
}