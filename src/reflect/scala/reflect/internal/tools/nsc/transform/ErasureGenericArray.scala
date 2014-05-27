package scala.reflect.internal
package tools.nsc.transform

//TODO-REFLECT remove object from Erasure and add dependency
trait ErasureGenericArray {
  val global: SymbolTable
  import global._
  import definitions._

  /** An extractor object for generic arrays */
  object GenericArray {

    /** Is `tp` an unbounded generic type (i.e. which could be instantiated
     *  with primitive as well as class types)?.
     */
    private def genericCore(tp: Type): Type = tp.dealiasWiden match {
      /* A Java Array<T> is erased to Array[Object] (T can only be a reference type), where as a Scala Array[T] is
       * erased to Object. However, there is only symbol for the Array class. So to make the distinction between
       * a Java and a Scala array, we check if the owner of T comes from a Java class.
       * This however caused issue SI-5654. The additional test for EXSITENTIAL fixes it, see the ticket comments.
       * In short, members of an existential type (e.g. `T` in `forSome { type T }`) can have pretty arbitrary
       * owners (e.g. when computing lubs, <root> is used). All packageClass symbols have `isJavaDefined == true`.
       */
      case TypeRef(_, sym, _) if sym.isAbstractType && (!sym.owner.isJavaDefined || sym.hasFlag(Flags.EXISTENTIAL)) =>
        tp
      case ExistentialType(tparams, restp) =>
        genericCore(restp)
      case _ =>
        NoType
    }

    /** If `tp` is of the form Array[...Array[T]...] where `T` is an abstract type
     *  then Some((N, T)) where N is the number of Array constructors enclosing `T`,
     *  otherwise None. Existentials on any level are ignored.
     */
    def unapply(tp: Type): Option[(Int, Type)] = tp.dealiasWiden match {
      case TypeRef(_, ArrayClass, List(arg)) =>
        genericCore(arg) match {
          case NoType =>
            unapply(arg) match {
              case Some((level, core)) => Some((level + 1, core))
              case None => None
            }
          case core =>
            Some((1, core))
        }
      case ExistentialType(tparams, restp) =>
        unapply(restp)
      case _ =>
        None
    }
  }
}