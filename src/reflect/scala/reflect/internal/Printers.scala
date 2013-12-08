/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

// todo. we need to unify this prettyprinter with NodePrinters

package scala
package reflect
package internal

import java.io.{ OutputStream, PrintWriter, StringWriter, Writer }
import Flags._
import scala.compat.Platform.EOL

trait Printers extends api.Printers { self: SymbolTable =>

  //nsc import treeInfo.{ IsTrue, IsFalse }

  final val showOuterTests = false

  /** Adds backticks if the name is a scala keyword. */
  def quotedName(name: Name, decode: Boolean): String = {
    val s = if (decode) name.decode else name.toString
    val term = name.toTermName
    if (nme.keywords(term) && term != nme.USCOREkw) "`%s`" format s
    else s
  }
  def quotedName(name: Name): String = quotedName(name, decode = false)
  def quotedName(name: String): String = quotedName(newTermName(name), decode = false)

  private def symNameInternal(tree: Tree, name: Name, decoded: Boolean): String = {
    val sym     = tree.symbol
    def qname   = quotedName(name.dropLocal, decoded)
    def qowner  = quotedName(sym.owner.name.dropLocal, decoded)
    def qsymbol = quotedName(sym.nameString)

    if (sym.name.toTermName == nme.ERROR)
      s"<$qname: error>"
    else if (sym == null || sym == NoSymbol)
      qname
    else if (sym.isMixinConstructor)
      s"/*$qowner*/$qsymbol"
    else
      qsymbol
  }

  def decodedSymName(tree: Tree, name: Name) = symNameInternal(tree, name, decoded = true)
  def symName(tree: Tree, name: Name) = symNameInternal(tree, name, decoded = false)

  /** Turns a path into a String, introducing backquotes
   *  as necessary.
   */
  def backquotedPath(t: Tree): String = {
    t match {
      case Select(qual, name) if name.isTermName  => "%s.%s".format(backquotedPath(qual), symName(t, name))
      case Select(qual, name) if name.isTypeName  => "%s#%s".format(backquotedPath(qual), symName(t, name))
      case Ident(name)                            => symName(t, name)
      case _                                      => t.toString
    }
  }

  class TreePrinter(out: PrintWriter) extends super.TreePrinter {
    protected var indentMargin = 0
    protected val indentStep = 2
    protected var indentString = "                                        " // 40

    printTypes = settings.printtypes.value
    printIds = settings.uniqid.value
    printKinds = settings.Yshowsymkinds.value
    printMirrors = false // typically there's no point to print mirrors inside the compiler, as there is only one mirror there
    printPositions = settings.Xprintpos.value

    def indent() = indentMargin += indentStep
    def undent() = indentMargin -= indentStep

    def printPosition(tree: Tree) = if (printPositions) print(tree.pos.show)

    def println() {
      out.println()
      while (indentMargin > indentString.length())
        indentString += indentString
      if (indentMargin > 0)
        out.write(indentString, 0, indentMargin)
    }

    def printSeq[a](ls: List[a])(printelem: a => Unit)(printsep: => Unit) {
      ls match {
        case List() =>
        case List(x) => printelem(x)
        case x :: rest => printelem(x); printsep; printSeq(rest)(printelem)(printsep)
      }
    }

    def printColumn(ts: List[Tree], start: String, sep: String, end: String) {
      print(start); indent(); println()
      printSeq(ts){print(_)}{print(sep); println()}; undent(); println(); print(end)
    }

    def printRow(ts: List[Tree], start: String, sep: String, end: String) {
      print(start); printSeq(ts){print(_)}{print(sep)}; print(end)
    }

    def printRow(ts: List[Tree], sep: String) { printRow(ts, "", sep, "") }

    def printTypeParams(ts: List[TypeDef]) {
      if (!ts.isEmpty) {
        print("["); printSeq(ts){ t =>
          printAnnotations(t)
          printParam(t)
        }{print(", ")}; print("]")
      }
    }

    def printLabelParams(ps: List[Ident]) {
      print("(")
      printSeq(ps){printLabelParam}{print(", ")}
      print(")")
    }

    def printLabelParam(p: Ident) {
      print(symName(p, p.name)); printOpt(": ", TypeTree() setType p.tpe)
    }

    def printValueParams(ts: List[ValDef]) {
      print("(")
      if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
      printSeq(ts){printParam}{print(", ")}
      print(")")
    }

    def printParam(tree: Tree) {
      tree match {
        case ValDef(mods, name, tp, rhs) =>
          printPosition(tree)
          printAnnotations(tree)
          print(symName(tree, name)); printOpt(": ", tp); printOpt(" = ", rhs)
        case TypeDef(mods, name, tparams, rhs) =>
          printPosition(tree)
          print(symName(tree, name))
          printTypeParams(tparams); print(rhs)
      }
    }

    def printBlock(tree: Tree) {
      tree match {
        case Block(_, _) =>
          print(tree)
        case _ =>
          printColumn(List(tree), "{", ";", "}")
      }
    }

    private def symFn[T](tree: Tree, f: Symbol => T, orElse: => T): T = tree.symbol match {
      case null | NoSymbol  => orElse
      case sym              => f(sym)
    }
    private def ifSym(tree: Tree, p: Symbol => Boolean) = symFn(tree, p, false)

    def printOpt(prefix: String, tree: Tree) {
      if (!tree.isEmpty) { print(prefix, tree) }
    }

    def printModifiers(tree: Tree, mods: Modifiers): Unit = printFlags(
       if (tree.symbol == NoSymbol) mods.flags else tree.symbol.flags, "" + (
         if (tree.symbol == NoSymbol) mods.privateWithin
         else if (tree.symbol.hasAccessBoundary) tree.symbol.privateWithin.name
         else ""
      )
    )

    def printFlags(flags: Long, privateWithin: String) {
      val mask: Long = if (settings.debug) -1L else PrintableFlags
      val s = flagsToString(flags & mask, privateWithin)
      if (s != "") print(s + " ")
    }

    def printAnnotations(tree: Tree) {
      // SI-5885: by default this won't print annotations of not yet initialized symbols
      val annots = tree.symbol.annotations match {
        case Nil  => tree.asInstanceOf[MemberDef].mods.annotations
        case anns => anns
      }
      annots foreach (annot => print("@"+annot+" "))
    }

    private var currentOwner: Symbol = NoSymbol
    private var selectorType: Type = NoType

    def printTree(tree: Tree) {
      tree match {
        case EmptyTree =>
          print("<empty>")

        case ClassDef(mods, name, tparams, impl) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          val word =
            if (mods.isTrait) "trait"
            else if (ifSym(tree, _.isModuleClass)) "object"
            else "class"

          print(word, " ", symName(tree, name))
          printTypeParams(tparams)
          print(if (mods.isDeferred) " <: " else " extends ", impl)

        case PackageDef(packaged, stats) =>
          printAnnotations(tree)
          print("package ", packaged); printColumn(stats, " {", ";", "}")

        case ModuleDef(mods, name, impl) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print("object " + symName(tree, name), " extends ", impl)

        case ValDef(mods, name, tp, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print(if (mods.isMutable) "var " else "val ", symName(tree, name))
          printOpt(": ", tp)
          if (!mods.isDeferred)
            print(" = ", if (rhs.isEmpty) "_" else rhs)

        case DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print("def " + symName(tree, name))
          printTypeParams(tparams); vparamss foreach printValueParams
          printOpt(": ", tp); printOpt(" = ", rhs)

        case TypeDef(mods, name, tparams, rhs) =>
          if (mods hasFlag (PARAM | DEFERRED)) {
            printAnnotations(tree)
            printModifiers(tree, mods); print("type "); printParam(tree)
          } else {
            printAnnotations(tree)
            printModifiers(tree, mods); print("type " + symName(tree, name))
            printTypeParams(tparams); printOpt(" = ", rhs)
          }

        case LabelDef(name, params, rhs) =>
          print(symName(tree, name)); printLabelParams(params); printBlock(rhs)

        case Import(expr, selectors) =>
          // Is this selector remapping a name (i.e, {name1 => name2})
          def isNotRemap(s: ImportSelector) : Boolean = (s.name == nme.WILDCARD || s.name == s.rename)
          def selectorToString(s: ImportSelector): String = {
            val from = quotedName(s.name)
            if (isNotRemap(s)) from
            else from + "=>" + quotedName(s.rename)
          }
          print("import ", backquotedPath(expr), ".")
          selectors match {
            case List(s) =>
              // If there is just one selector and it is not remapping a name, no braces are needed
              if (isNotRemap(s)) print(selectorToString(s))
              else print("{", selectorToString(s), "}")
              // If there is more than one selector braces are always needed
            case many =>
              print(many.map(selectorToString).mkString("{", ", ", "}"))
          }

       case Template(parents, self, body) =>
          val currentOwner1 = currentOwner
          if (tree.symbol != NoSymbol) currentOwner = tree.symbol.owner
//          if (parents exists isReferenceToAnyVal) {
//            print("AnyVal")
//          }
//          else {
          printRow(parents, " with ")
          if (!body.isEmpty) {
            if (self.name != nme.WILDCARD) {
              print(" { ", self.name); printOpt(": ", self.tpt); print(" => ")
            } else if (!self.tpt.isEmpty) {
              print(" { _ : ", self.tpt, " => ")
            } else {
              print(" {")
            }
            printColumn(body, "", ";", "}")
          }
//          }
          currentOwner = currentOwner1

        case Block(stats, expr) =>
          printColumn(stats ::: List(expr), "{", ";", "}")

        case Match(selector, cases) =>
          val selectorType1 = selectorType
          selectorType = selector.tpe
          print(selector); printColumn(cases, " match {", "", "}")
          selectorType = selectorType1

        case CaseDef(pat, guard, body) =>
          print("case ")
          def patConstr(pat: Tree): Tree = pat match {
            case Apply(fn, args) => patConstr(fn)
            case _ => pat
          }
          if (showOuterTests &&
              needsOuterTest(
                patConstr(pat).tpe.finalResultType, selectorType, currentOwner))
            print("???")
          print(pat); printOpt(" if ", guard)
          print(" => ", body)

        case Alternative(trees) =>
          printRow(trees, "(", "| ", ")")

        case Star(elem) =>
          print("(", elem, ")*")

        case Bind(name, t) =>
          print("(", symName(tree, name), " @ ", t, ")")

        case UnApply(fun, args) =>
          print(fun, " <unapply> "); printRow(args, "(", ", ", ")")

        case ArrayValue(elemtpt, trees) =>
          print("Array[", elemtpt); printRow(trees, "]{", ", ", "}")

        case Function(vparams, body) =>
          print("("); printValueParams(vparams); print(" => ", body, ")")
          if (printIds && tree.symbol != null) print("#"+tree.symbol.id)

        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case AssignOrNamedArg(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case If(cond, thenp, elsep) =>
          print("if (", cond, ")"); indent(); println()
          print(thenp); undent()
          if (!elsep.isEmpty) {
            println(); print("else"); indent(); println(); print(elsep); undent()
          }

        case Return(expr) =>
          print("return ", expr)

        case Try(block, catches, finalizer) =>
          print("try "); printBlock(block)
          if (!catches.isEmpty) printColumn(catches, " catch {", "", "}")
          printOpt(" finally ", finalizer)

        case Throw(expr) =>
          print("throw ", expr)

        case New(tpe) =>
          print("new ", tpe)

        case Typed(expr, tp) =>
          print("(", expr, ": ", tp, ")")

        case TypeApply(fun, targs) =>
          print(fun); printRow(targs, "[", ", ", "]")

        case Apply(fun, vargs) =>
          print(fun); printRow(vargs, "(", ", ", ")")

        case ApplyDynamic(qual, vargs) =>
          print("<apply-dynamic>(", qual, "#", tree.symbol.nameString)
          printRow(vargs, ", (", ", ", "))")

        case Super(This(qual), mix) =>
          if (!qual.isEmpty || tree.symbol != NoSymbol) print(symName(tree, qual) + ".")
          print("super")
          if (!mix.isEmpty)
            print("[" + mix + "]")

        case Super(qual, mix) =>
          print(qual, ".super")
          if (!mix.isEmpty)
            print("[" + mix + "]")

        case This(qual) =>
          if (!qual.isEmpty) print(symName(tree, qual) + ".")
          print("this")

        case Select(qual @ New(tpe), name) if !settings.debug =>
          print(qual)

        case Select(qualifier, name) =>
          print(backquotedPath(qualifier), ".", symName(tree, name))

        case id @ Ident(name) =>
          val str = symName(tree, name)
          print( if (id.isBackquoted) "`" + str + "`" else str )

        case Literal(x) =>
          print(x.escapedStringValue)

        case tt: TypeTree =>
          if ((tree.tpe eq null) || (printPositions && tt.original != null)) {
            if (tt.original != null) print("<type: ", tt.original, ">")
            else print("<type ?>")
          } else if ((tree.tpe.typeSymbol ne null) && tree.tpe.typeSymbol.isAnonymousClass) {
            print(tree.tpe.typeSymbol.toString)
          } else {
            print(tree.tpe.toString)
          }

        case Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
          def printAnnot() {
            print("@", tpt)
            if (!args.isEmpty)
              printRow(args, "(", ",", ")")
          }
          print(tree, if (tree.isType) " " else ": ")
          printAnnot()

        case SingletonTypeTree(ref) =>
          print(ref, ".type")

        case SelectFromTypeTree(qualifier, selector) =>
          print(qualifier, "#", symName(tree, selector))

        case CompoundTypeTree(templ) =>
          print(templ)

        case AppliedTypeTree(tp, args) =>
          print(tp); printRow(args, "[", ", ", "]")

        case TypeBoundsTree(lo, hi) =>
          // Avoid printing noisy empty typebounds everywhere
          // Untyped empty bounds are not printed by printOpt,
          // but after they are typed we have to exclude Nothing/Any.
          if ((lo.tpe eq null) || !(lo.tpe =:= definitions.NothingTpe))
            printOpt(" >: ", lo)

          if ((hi.tpe eq null) || !(hi.tpe =:= definitions.AnyTpe))
            printOpt(" <: ", hi)

        case ExistentialTypeTree(tpt, whereClauses) =>
          print(tpt)
          printColumn(whereClauses, " forSome { ", ";", "}")

// SelectFromArray is no longer visible in scala.reflect.internal.
// eliminated until we figure out what we will do with both Printers and
// SelectFromArray.
//          case SelectFromArray(qualifier, name, _) =>
//          print(qualifier); print(".<arr>"); print(symName(tree, name))

        case tree =>
          xprintTree(this, tree)
      }
      if (printTypes && tree.isTerm && tree.canHaveAttrs) {
        print("{", if (tree.tpe eq null) "<null>" else tree.tpe.toString, "}")
      }
    }

    def print(args: Any*): Unit = args foreach {
      case tree: Tree =>
        printPosition(tree)
        printTree(tree)
      case name: Name =>
        print(quotedName(name))
      case arg =>
        out.print(if (arg == null) "null" else arg.toString)
    }
  }

  class ParsedTreePrinter(out: PrintWriter, printMultiline: Boolean = false, decodeNames: Boolean = true) extends TreePrinter(out) {
      //TODO maybe we need to pass this stack when explicitly run show inside print
      val contextStack = scala.collection.mutable.Stack[Tree]()

      protected def compareNames(name1: Name, name2: Name) =
        !Option(name1).isEmpty && !Option(name2).isEmpty && (name1.toString.trim == name2.toString.trim)

      override def printModifiers(tree: Tree, mods: Modifiers): Unit = printModifiers(tree, mods, false)

      def printModifiers(tree: Tree, mods: Modifiers, isCtr: Boolean): Unit =
        if (getCurrentContext().isEmpty || modsAccepted)
          printFlags(mods.flags, "" + mods.privateWithin, isCtr)
        else
          List(IMPLICIT, CASE, LAZY).foreach{flag => if(mods.hasFlag(flag))  printFlags(flag, "", isCtr)}

      def modsAccepted = getCurrentContext() map {
        case _:ClassDef | _:ModuleDef | _:Template | _:PackageDef => true
        case _ => false
      } getOrElse false

      override def printFlags(flags: Long, privateWithin: String) =
        printFlags(flags, privateWithin, false)

      def printFlags(flags: Long, privateWithin: String, isCtr: Boolean) {
        val base = PROTECTED | OVERRIDE | PRIVATE | ABSTRACT | FINAL | SEALED | LAZY | LOCAL
        val mask = if (isCtr) base else base | IMPLICIT

        val s = flagsToString(flags & mask, privateWithin)
        if (s != "") print(s + " ")
        //case flag should be the last
        val caseFlag = flagsToString(flags & CASE)
        if (!caseFlag.isEmpty) print(caseFlag + " ")
        //abs override flag should be the last
        val absOverrideFlag = flagsToString(flags & ABSOVERRIDE)
        if (!absOverrideFlag.isEmpty) print("abstract override ")
      }

      def printConstrParams(ts: List[ValDef], isConstr: Boolean) {
        codeInParantheses(){
          if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
          printSeq(ts) {
            printParam(_, true)
          } { print(", ") }
        }
      }

      override def printValueParams(ts: List[ValDef]) {
        printValueParams(ts, false)
      }

      def printValueParams(ts: List[ValDef], isFuncTree: Boolean) {
        //val a: Int => Int = implicit x => x //parantheses are not allowed here
        val printParanthesis = !isFuncTree || {
          ts match {
            case List(vd: ValDef) => !vd.mods.hasFlag(IMPLICIT)
            case _ => true
          }
        }

        if (printParanthesis)
          super.printValueParams(ts)
        else {
          if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
          printSeq(ts) {
            printParam
          } { print(", ") }
        }
      }

      def printParam(tree: Tree, isConstr: Boolean) {
        tree match {
          case ValDef(mods, name, tp, rhs) =>
  //          printPosition(tree)
            printAnnotations(tree)
            if (isConstr) {
              printModifiers(tree, mods, isConstr)
            }
            print(if (mods.isMutable && isConstr) "var " else if (isConstr) "val " else "", symbName(tree, name));
            if (name.endsWith("_")) print(" ");
            printOpt(": ", tp);
            printOpt(" = ", rhs)
          case _ => super.printParam(tree)
        }
      }

      override def printParam(tree: Tree) {
        printParam(tree, false)
      }

      override def printAnnotations(tree: Tree) {
        val annots = tree.asInstanceOf[MemberDef].mods.annotations
        annots foreach {
          case Apply(Select(New(tree), p), args) => val ap = Apply(tree, args)
            print("@", ap, " ")
          case ann => print("@" + ann + " ")
        }
      }

      override def printTypeParams(ts: List[TypeDef]) {
        if (!ts.isEmpty) {
          print("["); printSeq(ts){ t =>
            printAnnotations(t)
            if (t.mods.hasFlag(CONTRAVARIANT)) {
              print("-")
            } else if (t.mods.hasFlag(COVARIANT)) {
              print("+")
            }
            printParam(t)
          }{print(", ")}; print("]")
        }
      }

      def codeInParantheses(condition: Boolean = true)(body: =>Unit) {
        if (condition) print("(")
        body
        if (condition) print(")")
      }

      def specialTreeContext(context: Tree)(iIf: Boolean = true, iMatch: Boolean = true,
          iTry: Boolean = true, iAnnotated: Boolean = true, iBlock: Boolean = true, iLabelDef: Boolean = true) = {
        context match {
          case _: If => iIf
          case _: Match => iMatch
          case _: Try => iTry
          case _: Annotated => iAnnotated
          case _: Block => iBlock
          case _: LabelDef => iLabelDef
          case _ => false
        }
      }

      protected def isIntLitWithDecodedOp(qual: Tree, name: Name) = {
        lazy val qualIsIntLit = qual match {
          case Literal(x) => x.value.isInstanceOf[Int]
          case _ => false
        }
        decodeNames && qualIsIntLit && name.isOperatorName
      }

      //Danger while using inheritance: it's hidden (overwritten) method
      def backquotedPath(t: Tree): String = {
        t match {
          case Select(qual, name) if (name.isTermName && specialTreeContext(qual)(iLabelDef = false)) || isIntLitWithDecodedOp(qual, name) => "(%s).%s".format(backquotedPath(qual), symbName(t, name))
          case Select(qual, name) if name.isTermName  => "%s.%s".format(backquotedPath(qual), symbName(t, name))
          case Select(qual, name) if name.isTypeName  => "%s#%s".format(backquotedPath(qual), symbName(t, name))
          case Ident(name)                            => symbName(t, name)
          case _                                      => render(t, treeToCodePrinter(_))
        }
      }

      def contextManaged(context: Tree)(body: =>Unit) {
        contextStack.push(context)
        body
        contextStack.pop()
      }

      def getCurrentContext() = if (!contextStack.isEmpty) Some(contextStack.top) else None

      def removeDefaultTypesFromList(trees: List[Tree])(classesToRemove: List[String])(traitsToRemove: List[String]) =
        removeDefaultTraitsFromList(removeDefaultClassesFromList(trees, classesToRemove), traitsToRemove)

      def removeDefaultClassesFromList(trees: List[Tree], classesToRemove: List[String]) = trees filter {
        case Select(Ident(sc), name) => !((classesToRemove.contains(name.toString)) && (sc.toString == "scala"))
        case _ => true
      }

      def removeDefaultTraitsFromList(trees: List[Tree], traitsToRemove: List[String]): List[Tree] =
        trees match {
          case Nil => trees
          case list : List[Tree] => list.last match {
            case Select(Ident(sc), name) if ((traitsToRemove.contains(name.toString)) && (sc.toString == "scala"))
              => removeDefaultTraitsFromList(list.init, traitsToRemove)
            case _ => list
           }
        }

      def getPrimaryConstr(methods: List[Tree]) =
        methods collectFirst {
          case dd: DefDef if dd.name.toString.trim == nme.CONSTRUCTOR.toString.trim => dd
        }

      // Is this selector remapping a name (i.e, {name1 => name2})
      protected def isNotRemap(s: ImportSelector): Boolean =
        (compareNames(s.name, nme.WILDCARD) || compareNames(s.name, s.rename))

      protected def selectorToString(s: ImportSelector): String = {
        val from = quotedName(s.name)
        if (isNotRemap(s)) from
        else from + "=>" + quotedName(s.rename)
      }

      override def printTree(tree: Tree) {
        tree match {
          case ClassDef(mods, name, tparams, impl) =>
            contextManaged(tree){
              printAnnotations(tree)
              val word =
                if (mods.isTrait){
                  printModifiers(tree, mods &~ ABSTRACT) // avoid abstract modifier for traits
                  "trait"
                } else {
                  printModifiers(tree, mods)
                  "class"
                }

              print(word, " ", symbName(tree, name))
              printTypeParams(tparams)

              val Template(parents @ List(_*), self, methods) = impl
              if (!mods.isTrait) {
                val templateVals = methods collect {
                  case ValDef(mods, name, _, _) => (name, mods)
                }

                val primaryConstrOpt = getPrimaryConstr(methods)

                primaryConstrOpt map {
                  primaryConstr =>

                  val cstrMods = primaryConstr.mods
                  val vparamss = primaryConstr.vparamss

                  //combine modifiers
                  val printParamss =
                    vparamss map {
                      vparams =>
                        if (vparams.isEmpty) vparams
                        else vparams map {
                          vparam =>
                            templateVals find {
                              tv =>
                                compareNames(tv._1, vparam.name)
                            } map {
                              templateVal =>
                                ValDef(Modifiers(vparam.mods.flags | templateVal._2.flags, templateVal._2.privateWithin,
                                  (vparam.mods.annotations ::: templateVal._2.annotations) distinct), vparam.name, vparam.tpt, vparam.rhs)
                            } getOrElse vparam
                        }
                      }

                  //constructor's modifier
                  if (cstrMods.hasFlag(AccessFlags)) {
                    print(" ")
                    printModifiers(primaryConstr, cstrMods)
                  }

                  //constructor's params
                  printParamss foreach { printParams =>
                    //don't print single empty constructor param list
                    if (!(printParams.isEmpty && printParamss.size == 1) || cstrMods.hasFlag(AccessFlags)) {
                      printConstrParams(printParams, true)
                      print(" ")
                    }
                  }
                    //case classes without a parameter list are not allowed
                    if ((printParamss.isEmpty || printParamss(0).isEmpty) && mods.isCase) print("() ")
                } getOrElse (print(" "))

              }

              //get trees without default classes and traits (when they are last)
              val printedParents = removeDefaultTypesFromList(parents)(List("AnyRef"))(if (mods.hasFlag(CASE)) List("Product", "Serializable") else Nil)

              print(if (mods.isDeferred) "<: " else if (!printedParents.isEmpty) " extends "
                else "", impl)
            }

          case PackageDef(packaged, stats) =>
            contextManaged(tree){
              packaged match {
                case Ident(name) if compareNames(name, nme.EMPTY_PACKAGE_NAME) =>
                  printSeq(stats) {
                    print(_)
                  } {
                    print(";");
                    println()
                  };
                case _ =>
                  printAnnotations(tree)
                  print("package ", packaged);
                  printColumn(stats, " {", "\n", "}")
              }
            }

          case ModuleDef(mods, name, impl) =>
            contextManaged(tree){
              printAnnotations(tree)
              printModifiers(tree, mods);
              val Template(parents @ List(_*), self, methods) = impl
              val parentsWAnyRef = removeDefaultClassesFromList(parents, List("AnyRef"))
              print("object " + symbName(tree, name), if (!parentsWAnyRef.isEmpty) " extends " else "", impl)
            }

          case vd@ValDef(mods, name, tp, rhs) =>
            printAnnotations(tree)
            printModifiers(tree, mods)
            print(if (mods.isMutable) "var " else "val ", symbName(tree, name))
            if (name.endsWith("_")) print(" ")

            printOpt(
              // place space after symbolic def name (val *: Unit does not compile)
              (if(symbName(tree, name) != symbName(tree, name,false) || symbName(tree, name) != symbName(tree, name, true))
                " "
              else
                "") +
              ": ",
              tp
            )
            contextManaged(tree){
              if (!mods.isDeferred)
                print(" = ", if (rhs.isEmpty) "_" else rhs)
            }

          case dd@DefDef(mods, name, tparams, vparamss, tp, rhs) =>
            printAnnotations(tree)
            printModifiers(tree, mods)
            print("def " + symbName(tree, name))
            printTypeParams(tparams);
            vparamss foreach printValueParams
            if (tparams.isEmpty && (vparamss.isEmpty || vparamss(0).isEmpty) && name.endsWith("_"))
              print(" ")
            printOpt(
              // place space after symbolic def name (def *: Unit does not compile)
              (if(symbName(tree, name) != symbName(tree, name,false) || symbName(tree, name) != symbName(tree, name, true))
                " "
              else
                "") +
              ": ",
              tp
            )
            contextManaged(tree){
              printOpt(" = " + (if (mods.hasFlag(MACRO)) "macro " else ""), rhs)
            }

          case td@TypeDef(mods, name, tparams, rhs) =>
            if (mods hasFlag (PARAM | DEFERRED)) {
              printAnnotations(tree)
              printModifiers(tree, mods);
              print("type ");
              printParam(tree)
            } else {
              printAnnotations(tree)
              printModifiers(tree, mods);
              print("type " + symbName(tree, name))
              printTypeParams(tparams);
              contextManaged(tree){
                printOpt(" = ", rhs)
              }
            }

          case LabelDef(name, params, rhs) =>
            if (name.toString.contains("while$")) {
              contextManaged(tree){
                val If(cond, thenp, elsep) = rhs
                print("while (", cond, ") ")
                val Block(list, wh) = thenp
                printColumn(list, "", ";", "")
              }
            } else if (name.toString.contains("doWhile$")) {
              contextManaged(tree){
                val Block(bodyList: List[Tree], ifCond @ If(cond, thenp, elsep)) = rhs
                print("do ")
                printColumn(bodyList, "", ";", "")
                print(" while (", cond, ") ")
              }
            } else {
              print(symbName(tree, name)); printLabelParams(params);
              contextManaged(tree){
                printBlock(rhs)
              }
            }

          case Import(expr, selectors) =>
            print("import ", backquotedPath(expr), ".")
            selectors match {
              case List(s) =>
                // If there is just one selector and it is not remapping a name, no braces are needed
                if (isNotRemap(s)) print(selectorToString(s))
                else print("{", selectorToString(s), "}")
              // If there is more than one selector braces are always needed
              case many =>
                print(many.map(selectorToString).mkString("{", ", ", "}"))
            }

          case Template(parents, self, body) =>

            val printedParents =
              getCurrentContext() map {
                //val example: Option[AnyRef => Product1[Any] with AnyRef] = ... - CompoundTypeTree with template
                case _: CompoundTypeTree => parents
                case ClassDef(mods, name, _, _) if mods.hasFlag(CASE) => removeDefaultTypesFromList(parents)(List("AnyRef"))(List("Product", "Serializable"))
                case _ => removeDefaultClassesFromList(parents, List("AnyRef"))
              } getOrElse(parents)

            val primaryCtrOpt = getPrimaryConstr(body)
            var ap: Option[Apply] = None

            for (primaryCtr <- primaryCtrOpt) {
              primaryCtr match {
                case DefDef(_, _, _, _, _, Block(ctBody @ List(_*), _)) =>
                  ap = ctBody collectFirst {
                    case apply: Apply => apply
                  }

                  //vals in preinit blocks
                  val presuperVals = ctBody filter {
                    case vd:ValDef => vd.mods.hasFlag(PRESUPER)
                    case _ => false
                  }

                  if (!presuperVals.isEmpty) {
                    print("{")
                    printColumn(presuperVals, "", ";", "")
                    print("} " + (if (!printedParents.isEmpty) "with " else ""))
                  }

                case _ =>
              }
            }

            if (!printedParents.isEmpty) {
              val (clParent :: traits) = printedParents
              print(clParent)

              def getConstrParams(tree: Tree, cargs: List[List[Tree]]): List[List[Tree]] = {
                tree match {
                  case Apply(inTree, args) =>
                    getConstrParams(inTree, cargs):+args
                  case _ => cargs
                }
              }

              val applyParamsList = ap map {getConstrParams(_, Nil)} getOrElse Nil
              applyParamsList foreach {x: List[Tree] => if (!(x.isEmpty && applyParamsList.size == 1)) printRow(x, "(", ", ", ")")}

              if (!traits.isEmpty) {
                printRow(traits, " with ", " with ", "")
              }
            }
            //remove primary constr def and constr val and var defs
            //right contains all constructors
            //TODO see impl filter on Tree
            val (left, right) = body.filter {
              //remove valdefs defined in constructor and pre-init block
              case vd: ValDef => !vd.mods.hasFlag(PARAMACCESSOR) && !vd.mods.hasFlag(PRESUPER)
              case dd: DefDef => !compareNames(dd.name, nme.MIXIN_CONSTRUCTOR) //remove $this$ from traits
              case EmptyTree => false
              case _ => true
            } span {
              case dd: DefDef => !compareNames(dd.name, nme.CONSTRUCTOR)
              case _ => true
            }

            val modBody = left ::: right.drop(1)//List().drop(1) ==> List()
            val showBody = !(modBody.isEmpty &&
              (self match {
                case ValDef(mods, name, TypeTree(), rhs) if (mods & PRIVATE) != 0 && name.decoded == "_" && rhs.isEmpty => true // workaround for superfluous ValDef when parsing class without body using quasi quotes
                case _ => self.isEmpty
              }))
            if (showBody) {
              if (!compareNames(self.name, nme.WILDCARD)) {
                print(" { ", self.name);
                printOpt(": ", self.tpt);
                print(" =>")
              } else if (!self.tpt.isEmpty) {
                print(" { _ : ", self.tpt, " =>")
              } else {
                print(" {")
              }
              contextManaged(tree) {
                printColumn(modBody, "", ";", "}")
              }
            }

          case Block(stats, expr) =>
            contextManaged(tree){
              printColumn(stats ::: List(expr), "{", ";", "}")
            }

          case Match(selector, cases) =>
            //insert braces if match is inner
            //make this function available for other casses
            //passing required type for checking
            def insertBraces(body: =>Unit) {
              if (contextStack.exists{
                _.isInstanceOf[Match]
              }) {
                  print("(")
                  body
                  print(")")
              } else body
            }

            val printParantheses = specialTreeContext(selector)(iLabelDef = false)
            tree match {
              case Match(EmptyTree, cs) =>
                printColumn(cases, "{", "", "}")
              case _ =>
                insertBraces {
                  contextManaged(tree){
                    codeInParantheses(printParantheses) {
                      print(selector);
                    }
                  }
                  printColumn(cases, " match {", "", "}")
                }
            }

          case CaseDef(pat, guard, body) =>
            print("case ")
            def patConstr(pat: Tree): Tree = pat match {
              case Apply(fn, args) => patConstr(fn)
              case _ => pat
            }

            print(pat);
            printOpt(" if ", guard)
            contextManaged(tree) {
              print(" => ", body)
            }

          case Star(elem) =>
            print(elem, "*")

          case Bind(name, t) =>
            if (t == EmptyTree) print("(", symbName(tree, name), ")")
            else if (t.exists{
              case _:Star => true
              case _ => false
            }) print(symbName(tree, name), " @ ", t)
            else print("(", symbName(tree, name), " @ ", t, ")")

          //almost the same as in original
          case Function(vparams, body) =>
            print("(");
            printValueParams(vparams, true);
            print(" => ", body, ")")

          case Typed(expr, tp) =>
            tp match {
              case Function(List(), EmptyTree) => print("(", expr, " _)") //func _
              case _ => print("((", expr, "): ", tp, ")") //parenteses required when (a match {}) : Type
            }

          case Apply(fun, vargs) =>
            //process methods ending on colon with multiple args list//
            //example:
  //          def t[A,B](as: List[A]) = {
  //            println("hello")
  //            ((Map.empty[B, List[A]]) /: as){ (nels, a) => println(""); (nels)}
  //          }
  //         by default results in:
  //        {
  //          val x$1 = Map.empty[B, List[A]];
  //          as.$div$colon(x$1)
  //        }(((nels, a) => {
  //          println("");
  //          nels
  //        }))
            tree match {
              //processing methods ending on colons (x \: list)
              case Apply(Block(l1 @ List(sVD :ValDef), a1 @ Apply(Select(_, methodName), l2 @ List(Ident(iVDName)))), l3 @ List(_*))
                if sVD.mods.hasFlag(SYNTHETIC) && methodName.toString.endsWith("$colon") && compareNames(sVD.name, iVDName) => //&& (sVD.name.toString.trim == iVDName.toString.trim) =>
                val printBlock = Block(l1, Apply(a1, l3))
                print(printBlock)
              case Apply(tree1, _) if (specialTreeContext(tree1)(iAnnotated = false)) => codeInParantheses(){print(fun)}; printRow(vargs, "(", ", ", ")")
              case _ => print(fun); printRow(vargs, "(", ", ", ")")
            }


          case Super(This(qual), mix) =>
            if (!qual.isEmpty || tree.symbol != NoSymbol) print(symbName(tree, qual) + ".")
            print("super")
            if (!mix.isEmpty)
              print("[" + mix + "]")

          case This(qual) =>
            //symName is redefined
            if (!qual.isEmpty) print(symbName(tree, qual) + ".")
            print("this")

          case Select(qual@New(tpe), name) =>
            print(qual)

          case Select(qualifier, name) => {
            val printParantheses = specialTreeContext(qualifier)(iAnnotated = false) || isIntLitWithDecodedOp(qualifier, name)
            if (printParantheses) print("(", backquotedPath(qualifier), ").", symbName(tree, name))
            else print(backquotedPath(qualifier), ".", symbName(tree, name))
          }

          case id@Ident(name) =>
            if (!name.isEmpty) {
              val str = symbName(tree, name)

              val strIsBackquoted = str.startsWith("`") && str.endsWith("`")

              print(if (id.isBackquoted && !strIsBackquoted) "`" + str + "`" else str)
            }
            else {
              print("")
            }

          case l@Literal(x) =>
            //TODO refactor multiline string processing - x.stringValue
            if (x.value.isInstanceOf[String] && printMultiline && x.stringValue.contains("\n") && !x.stringValue.contains("\"\"\"") && x.stringValue.size > 1) {
              val splitValue = x.stringValue.split('\n'.toString).toList
              val multilineStringValue = if (x.stringValue.endsWith("\n")) splitValue :+ "" else splitValue
              val trQuotes = "\"\"\""
              print(trQuotes); printSeq(multilineStringValue){print(_)}{print("\n")}; print(trQuotes)
            } else {
              //processing Float constants
              val printValue = x.escapedStringValue + (if (x.value.isInstanceOf[Float]) "F" else "") //correct printing of Float
              print(printValue)
            }

          case Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
            def printAnnot() {
              print("@", tpt)
              if (!args.isEmpty)
                printRow(args, "(", ",", ")")
            }

            val printParantheses = specialTreeContext(tree)()
            codeInParantheses(printParantheses){print(tree)}; print(if (tree.isType) " " else ": ")
            printAnnot()

          case SelectFromTypeTree(qualifier, selector) =>
            print("(", qualifier, ")#", symbName(tree, selector))

          case CompoundTypeTree(templ) =>
            contextManaged(tree){
              print(templ)
            }

          case AppliedTypeTree(tp, args) =>
            //it's possible to have (=> String) => String type but Function1[=> String, String] is not correct
            def containsByNameTypeParam =
              args exists {
                  case AppliedTypeTree(Select(qual, name), _) => name.toString.trim.equals("<byname>")
                  case _ => false
                }

            if (containsByNameTypeParam) {
              print("(")
              printRow(args.init, "(", ", ", ")")
              print(" => ", args.last, ")")
            } else {
              if (tp.exists {
                case Select(_, name) => compareNames(name, tpnme.REPEATED_PARAM_CLASS_NAME)
                case _ => false
              } && !args.isEmpty) {
                print(args(0), "*")
              } else if (tp match {
                case Select(_, name) => compareNames(name, tpnme.BYNAME_PARAM_CLASS_NAME)
                case _ => false
              }) {
                print("=> ", if (args.isEmpty) "()" else args(0))
              } else {
                print(tp);
                printRow(args, "[", ", ", "]")
              }
            }

          case ExistentialTypeTree(tpt, whereClauses) =>
            print("(", tpt);
            printColumn(whereClauses, " forSome { ", ";", "})")

          case tbt@TypeBoundsTree(lo, hi) => {
            val loDefault = "_root_.scala.Nothing"
            val hiDefault = "_root_.scala.Any"
            if (loDefault != lo.toString()) printOpt(" >: ", lo); if (hiDefault != hi.toString()) printOpt(" <: ", hi)
          }

          case emptyTree if emptyTree.toString == "<empty>" => // workaround as case EmptyTree does not work for all universes because of path depedent types

          case tree => super.printTree(tree)
        }
        //TODO remove
        if (printTypes && tree.isTerm && !tree.isEmpty) {
          print("{", if (tree.tpe eq null) "<null>" else tree.tpe.toString, "}")
        }
      }

      //Danger: it's overwritten method - can be problems with inheritance)
      def symbName(tree: Tree, name: Name, decoded: Boolean = decodeNames) = {
        val encName = name.encoded
        val decName = name.decoded
        def modifyEncoded(s: String) = if (decoded && (encName.contains("$u") ||
          (encName.contains("$") && decName.exists(ch => opSym.contains(ch)) && decName.exists(ch => !opSym.contains(ch)) && !excList.exists(str => decName.contains(str)))))
          "`%s`" format s else s

        if (compareNames(name, nme.CONSTRUCTOR)) "this"
        else modifyEncoded(quotedName(name, decoded))
      }

      val opSym = List('~', '=', '<', '>', '!', '#', '%', '^', '&', '|', '*', '/', '+', '-', ':', '\\', '?', '@')
      val excList = List("\\", "_*")

      override def print(args: Any*): Unit = {
        args foreach {
          arg =>
              arg match {
                case tree: Tree =>
                  printTree(tree)
                case name: Name =>
                  print(quotedName(name))
                case other => super.print(other)
              }
        }
      }
  }

  /** Hook for extensions */
  def xprintTree(treePrinter: TreePrinter, tree: Tree) =
    treePrinter.print(tree.productPrefix+tree.productIterator.mkString("(", ", ", ")"))

  //TODO pass options
  override def treeToCodePrinter(writer: PrintWriter): TreePrinter = new ParsedTreePrinter(writer)
  def newTreePrinter(writer: PrintWriter): TreePrinter = new TreePrinter(writer)
  def newTreePrinter(stream: OutputStream): TreePrinter = newTreePrinter(new PrintWriter(stream))
  def newTreePrinter(): TreePrinter = newTreePrinter(new PrintWriter(ConsoleWriter))

  /** A writer that writes to the current Console and
   * is sensitive to replacement of the Console's
   * output stream.
   */
  object ConsoleWriter extends Writer {
    override def write(str: String) { Console.print(str) }

    def write(cbuf: Array[Char], off: Int, len: Int) {
      write(new String(cbuf, off, len))
    }

    def close = { /* do nothing */ }
    def flush = { /* do nothing */ }
  }

  def newRawTreePrinter(writer: PrintWriter): RawTreePrinter = new RawTreePrinter(writer)

  // provides footnotes for types and mirrors
  import scala.collection.mutable.{Map, WeakHashMap, SortedSet}
  private val footnoteIndex = new FootnoteIndex
  private class FootnoteIndex {
    private val index = Map[Class[_], WeakHashMap[Any, Int]]()
    private def classIndex[T: ClassTag] = index.getOrElseUpdate(classTag[T].runtimeClass, WeakHashMap[Any, Int]())
    private val counters = Map[Class[_], Int]()
    private def nextCounter[T: ClassTag] = {
      val clazz = classTag[T].runtimeClass
      counters.getOrElseUpdate(clazz, 0)
      counters(clazz) = counters(clazz) + 1
      counters(clazz)
    }

    def mkFootnotes() = new Footnotes
    class Footnotes {
      private val footnotes = Map[Class[_], SortedSet[Int]]()
      private def classFootnotes[T: ClassTag] = footnotes.getOrElseUpdate(classTag[T].runtimeClass, SortedSet[Int]())

      def put[T: ClassTag](any: T): Int = {
        val index = classIndex[T].getOrElseUpdate(any, nextCounter[T])
        classFootnotes[T] += index
        index
      }

      def get[T: ClassTag]: List[(Int, Any)] =
        classFootnotes[T].toList map (fi => (fi, classIndex[T].find{ case (any, ii) => ii == fi }.get._1))

      def print[T: ClassTag](printer: Printers.super.TreePrinter): Unit = {
        val footnotes = get[T]
        if (footnotes.nonEmpty) {
          printer.print(EOL)
          footnotes.zipWithIndex foreach {
            case ((fi, any), ii) =>
              printer.print("[", fi, "] ", any)
              if (ii < footnotes.length - 1) printer.print(EOL)
          }
        }
      }
    }
  }

  // emits more or less verbatim representation of the provided tree
  class RawTreePrinter(out: PrintWriter) extends super.TreePrinter {
    private var depth = 0
    private var printTypesInFootnotes = true
    private var printingFootnotes = false
    private val footnotes = footnoteIndex.mkFootnotes()

    def print(args: Any*): Unit = {
      // don't print type footnotes if the argument is a mere type
      if (depth == 0 && args.length == 1 && args(0) != null && args(0).isInstanceOf[Type])
        printTypesInFootnotes = false

      depth += 1
      args foreach {
        case expr: Expr[_] =>
          print("Expr")
          if (printTypes) print(expr.staticType)
          print("(")
          print(expr.tree)
          print(")")
        case EmptyTree =>
          print("EmptyTree")
        case self.noSelfType =>
          print("noSelfType")
        case self.pendingSuperCall =>
          print("pendingSuperCall")
        case tree: Tree =>
          val hasSymbolField = tree.hasSymbolField && tree.symbol != NoSymbol
          val isError = hasSymbolField && (tree.symbol.name string_== nme.ERROR)
          printProduct(
            tree,
            preamble = _ => {
              if (printPositions) print(tree.pos.show)
              print(tree.productPrefix)
              if (printTypes && tree.tpe != null) print(tree.tpe)
            },
            body = {
              case name: Name =>
                if (isError) {
                  if (isError) print("<")
                  print(name)
                  if (isError) print(": error>")
                } else if (hasSymbolField) {
                  tree match {
                    case refTree: RefTree =>
                      if (tree.symbol.name != refTree.name) print("[", tree.symbol, " aka ", refTree.name, "]")
                      else print(tree.symbol)
                    case defTree: DefTree =>
                      print(tree.symbol)
                    case _ =>
                      print(tree.symbol.name)
                  }
                } else {
                  print(name)
                }
              case Constant(s: String) =>
                print("Constant(\"" + s + "\")")
              case Constant(null) =>
                print("Constant(null)")
              case Constant(value) =>
                print("Constant(" + value + ")")
              case arg =>
                print(arg)
            },
            postamble = {
              case tree @ TypeTree() if tree.original != null => print(".setOriginal(", tree.original, ")")
              case _ => // do nothing
            })
        case sym: Symbol =>
          if (sym == NoSymbol) print("NoSymbol")
          else if (sym.isStatic && (sym.isClass || sym.isModule)) print(sym.fullName)
          else print(sym.name)
          if (printIds) print("#", sym.id)
          if (printKinds) print("#", sym.abbreviatedKindString)
          if (printMirrors) print("%M", footnotes.put[scala.reflect.api.Mirror[_]](mirrorThatLoaded(sym)))
        case tag: TypeTag[_] =>
          print("TypeTag(", tag.tpe, ")")
        case tag: WeakTypeTag[_] =>
          print("WeakTypeTag(", tag.tpe, ")")
        case tpe: Type =>
          val defer = printTypesInFootnotes && !printingFootnotes
          if (defer) print("[", footnotes.put(tpe), "]")
          else tpe match {
            case NoType => print("NoType")
            case NoPrefix => print("NoPrefix")
            case _ => printProduct(tpe.asInstanceOf[Product])
          }
        case mods: Modifiers =>
          print("Modifiers(")
          if (mods.flags != NoFlags || mods.privateWithin != tpnme.EMPTY || mods.annotations.nonEmpty) print(show(mods.flags))
          if (mods.privateWithin != tpnme.EMPTY || mods.annotations.nonEmpty) { print(", "); print(mods.privateWithin) }
          if (mods.annotations.nonEmpty) { print(", "); print(mods.annotations); }
          print(")")
        case name: Name =>
          print(show(name))
        case scope: Scope =>
          print("Scope")
          printIterable(scope.toList)
        case list: List[_] =>
          print("List")
          printIterable(list)
        case product: Product =>
          printProduct(product)
        case arg =>
          out.print(arg)
      }
      depth -= 1
      if (depth == 0 && !printingFootnotes) {
        printingFootnotes = true
        footnotes.print[Type](this)
        footnotes.print[scala.reflect.api.Mirror[_]](this)
        printingFootnotes = false
      }
    }

    def printProduct(
      p: Product,
      preamble: Product => Unit = p => print(p.productPrefix),
      body: Any => Unit = print(_),
      postamble: Product => Unit = p => print("")): Unit =
    {
      preamble(p)
      printIterable(p.productIterator.toList, body = body)
      postamble(p)
    }

    def printIterable(
      iterable: List[_],
      preamble: => Unit = print(""),
      body: Any => Unit = print(_),
      postamble: => Unit = print("")): Unit =
    {
      preamble
      print("(")
      val it = iterable.iterator
      while (it.hasNext) {
        body(it.next())
        print(if (it.hasNext) ", " else "")
      }
      print(")")
      postamble
    }
  }

  def show(name: Name): String = name match {
    case tpnme.WILDCARD => "tpnme.WILDCARD"
    case tpnme.EMPTY => "tpnme.EMPTY"
    case tpnme.ERROR => "tpnme.ERROR"
    case tpnme.PACKAGE => "tpnme.PACKAGE"
    case tpnme.WILDCARD_STAR => "tpnme.WILDCARD_STAR"
    case nme.WILDCARD => "nme.WILDCARD"
    case nme.EMPTY => "nme.EMPTY"
    case nme.ERROR => "tpnme.ERROR"
    case nme.PACKAGE => "nme.PACKAGE"
    case nme.CONSTRUCTOR => "nme.CONSTRUCTOR"
    case nme.ROOTPKG => "nme.ROOTPKG"
    case _ =>
      val prefix = if (name.isTermName) "TermName(\"" else "TypeName(\""
      prefix + name.toString + "\")"
  }

  def show(flags: FlagSet): String = {
    if (flags == NoFlags) nme.NoFlags.toString
    else {
      val s_flags = new scala.collection.mutable.ListBuffer[String]
      def hasFlag(left: Long, right: Long): Boolean = (left & right) != 0
      for (i <- 0 to 63 if hasFlag(flags, 1L << i))
        s_flags += flagToString(1L << i).replace("<", "").replace(">", "").toUpperCase
      s_flags mkString " | "
    }
  }
}
