package scala
package reflect
package runtime

import internal.Flags
import java.lang.{Class => jClass, Package => jPackage}
import scala.collection.mutable
import scala.reflect.runtime.ReflectionUtils.scalacShouldntLoadClass
import scala.reflect.internal.Flags._

private[reflect] trait SymbolLoaders { self: SymbolTable =>

  /** The standard completer for top-level classes
   *  @param clazz   The top-level class
   *  @param module  The companion object of `clazz`
   *  Calling `complete` on this type will assign the infos of `clazz` and `module`
   *  by unpickling information from the corresponding Java class. If no Java class
   *  is found, a package is created instead.
   */
  class TopClassCompleter(clazz: Symbol, module: Symbol) extends SymLoader with FlagAssigningCompleter {
    markFlagsCompleted(clazz, module)(mask = ~TopLevelPickledFlags)
    override def complete(sym: Symbol) = {
      debugInfo("completing "+sym+"/"+clazz.fullName)
      assert(sym == clazz || sym == module || sym == module.moduleClass)
      slowButSafeEnteringPhaseNotLaterThan(picklerPhase) {
        val loadingMirror = mirrorThatLoaded(sym)
        val javaClass = loadingMirror.javaClass(clazz.javaClassName)
        loadingMirror.unpickleClass(clazz, module, javaClass)
        // NOTE: can't mark as thread-safe here, because unpickleClass might decide to delegate to FromJavaClassCompleter
        // if (!isCompilerUniverse) markAllCompleted(clazz, module)
      }
    }
    override def load(sym: Symbol) = complete(sym)
  }

  /** Create a class and a companion object, enter in enclosing scope,
   *  and initialize with a lazy type completer.
   *  @param owner   The owner of the newly created class and object
   *  @param name    The simple name of the newly created class
   *  @param completer  The completer to be used to set the info of the class and the module
   */
  protected def initAndEnterClassAndModule(owner: Symbol, name: TypeName, completer: (Symbol, Symbol) => LazyType) = {
    assert(!(name.toString endsWith "[]"), name)
    val clazz = owner.newClass(name)
    val module = owner.newModule(name.toTermName)
    // without this check test/files/run/t5256g and test/files/run/t5256h will crash
    // todo. reflection meeting verdict: need to enter the symbols into the first symbol in the owner chain that has a non-empty scope
    if (owner.info.decls != EmptyScope) {
      owner.info.decls enter clazz
      owner.info.decls enter module
    }
    initClassAndModule(clazz, module, completer(clazz, module))
    (clazz, module)
  }

  protected def setAllInfos(clazz: Symbol, module: Symbol, info: Type) = {
    List(clazz, module, module.moduleClass) foreach (_ setInfo info)
  }

  protected def initClassAndModule(clazz: Symbol, module: Symbol, completer: LazyType) =
    setAllInfos(clazz, module, completer)

  /** The type completer for packages.
   */
  class LazyPackageType extends LazyType with FlagAgnosticCompleter {
    override def complete(sym: Symbol) {
      assert(sym.isPackageClass)
      sym setInfo new ClassInfoType(List(), new PackageScope(sym), sym)
        // override def safeToString = pkgClass.toString
      openPackageModule(sym)
      markAllCompleted(sym)
    }
  }


  // Since runtime reflection doesn't have a luxury of enumerating all classes
  // on the classpath, it has to materialize symbols for top-level definitions
  // (packages, classes, objects) on demand.
  //
  // Someone asks us for a class named `foo.Bar`? Easy. Let's speculatively create
  // a package named `foo` and then look up `newTypeName("bar")` in its decls.
  // This lookup, implemented in `SymbolLoaders.PackageScope` tests the waters by
  // trying to to `Class.forName("foo.Bar")` and then creates a ClassSymbol upon
  // success (the whole story is a bit longer, but the rest is irrelevant here).
  //
  // That's all neat, but these non-deterministic mutations of the global symbol
  // table give a lot of trouble in multi-threaded setting. One of the popular
  // reflection crashes happens when multiple threads happen to trigger symbol
  // materialization multiple times for the same symbol, making subsequent
  // reflective operations stumble upon outrageous stuff like overloaded packages.
  //
  // Short of significantly changing SymbolLoaders I see no other way than just
  // to slap a global lock on materialization in runtime reflection.
  class PackageScope(pkgClass: Symbol) extends Scope(initFingerPrints = -1L) // disable fingerprinting as we do not know entries beforehand
      with SynchronizedScope {
    assert(pkgClass.isType)

    // materializing multiple copies of the same symbol in PackageScope is a very popular bug
    // this override does its best to guard against it
    override def enter[T <: Symbol](sym: T): T = {
      // workaround for SI-7728
      if (isCompilerUniverse) super.enter(sym)
      else {
        val existing = super.lookupEntry(sym.name)
        assert(existing == null || existing.sym.isMethod, s"pkgClass = $pkgClass, sym = $sym, existing = $existing")
        super.enter(sym)
      }
    }

    override def enterIfNew[T <: Symbol](sym: T): T = {
      val existing = super.lookupEntry(sym.name)
      if (existing == null) enter(sym)
      else existing.sym.asInstanceOf[T]
    }

    // package scopes need to synchronize on the GIL
    // because lookupEntry might cause changes to the global symbol table
    override def syncLockSynchronized[T](body: => T): T = gilSynchronized(body)
    private val negatives = new mutable.HashSet[Name]
    override def lookupEntry(name: Name): ScopeEntry = syncLockSynchronized {
//      println(s"===> looking for entry: " + name)
      val namecon = printLog && name.containsName("String")
      val clNameCon = printLog && name.containsName("String")
      val printForConstr = name.toString.contains("<init>")
      if (namecon) {
        println
        println(s"===> lookupEntry (${rootMirror.hashCode()}) {")
        println(s"  pkgClass: $pkgClass")
        println(s"  mirrorThatLoaded(pkgClass).hashCode: ${mirrorThatLoaded(pkgClass).hashCode()}")
        println("}")
      }
      val e = super.lookupEntry(name)
      if (self.isInstanceOf[TypecheckerUniverse] && printForConstr) {
        println(s"===> looking for Constr: $name")
        if (e != null) println(s"  clonedClazz.enclosingRootClass == rootMirror.RootClass: ${e.sym.enclosingRootClass == rootMirror.RootClass}") else println("  null")
        println(s"<=== looking for Constr: $name")
      }
      if (self.isInstanceOf[TypecheckerUniverse] && clNameCon) {
        println(s"===> looking for String: $name")
        if (e != null) println(s"  clonedClazz.enclosingRootClass == rootMirror.RootClass: ${e.sym.enclosingRootClass == rootMirror.RootClass}") else println("  null")
        println(s"<=== looking for String: $name")
      }
      val r = if (e != null) {
        if (namecon) println("--- if-1")
        e
      } else if (scalacShouldntLoadClass(name) || (negatives contains name)) {
        if (namecon) {
          println("--- else-1: {")
          println(s"  scalacShouldntLoadClass(name): ${scalacShouldntLoadClass(name)}")
          println(s"  negatives contains name: ${negatives contains name}")
          println(s"  negatives: $negatives")
          println("}")
        }
        null
      } else {
        if (namecon) println("--- else-2")
        val path =
          if (pkgClass.isEmptyPackageClass) name.toString
          else pkgClass.fullName + "." + name
        val currentMirror = mirrorThatLoaded(pkgClass)
        currentMirror.tryJavaClass(path) match {
          case Some(cls) =>
            val loadingMirror = currentMirror.mirrorDefining(cls)
            val (_, module) =
              if (loadingMirror eq currentMirror) {
                initAndEnterClassAndModule(pkgClass, name.toTypeName, new TopClassCompleter(_, _))
              } else {
                val origOwner = loadingMirror.packageNameToScala(pkgClass.fullName)
                val clazz = origOwner.info decl name.toTypeName
                val module = origOwner.info decl name.toTermName
                assert(clazz != NoSymbol)
                assert(module != NoSymbol)

                //TODO-REFLECT - write here simulation - rootMirror should be created (because we have other mirrors inside
                //or we can try to pass it
                //and set to not create the class
                //with null/or other original rootMirror classloader
                //or it's parents we should clone symbols
                //1) check if loadingMirror (currentMirror) are in parent sequence of (rootMirror)
                //2) check if class is already loaded (Class.forName(...) - we shouldn't get new mirror) - maybe we need new class Loader
                //3) check if java.String is loaded by boot classloader
                def cloneLoadedSymbols(multCls: ClassLoader, current: ClassLoader) = {
//                  println("=== in cloneLoadedSymbols ===")
                  current == multCls
                  //cls.contains(current)
                }
                var tuv = false
                val resSymbols @ (resClazz, resModule) = self match {
                  //TODO-REFLECT override rootClassLoader for TypecheckerUniverse and remove rootMirror overriding
                  //                  case tu: TypecheckerUniverse if (name.containsName("String") || name.containsName("Object")  || name.containsName("Predef")
                  //                    || name.containsName("Array") || name.containsName("Int") || name.containsName("Byte") || name.containsName("Char")) 
                  case tu: TypecheckerUniverse if (name.toString.contains("String") || name.toString.contains("Object") || name.toString.contains("Predef")
                    || name.toString.contains("Array") || name.toString.contains("Int") || name.toString.contains("Byte") || name.toString.contains("Char") /*|| name.toString.contains("Any")*/ ) 
                    && cloneLoadedSymbols(tu.multClassLoader, cls.getClassLoader()) =>
                    tuv = true
//                    println("!!!!!! TypecheckerUniverse")
                    if (tuv && clNameCon) {
                      println
                      println("*********************************")
                      println("******* in SymbolLoaders ********")
                      println("*********************************")
                    }
                    val clonedClazz = clazz.cloneSymbol(pkgClass)
                    val clonedModule = module.cloneSymbol(pkgClass)

                    (clonedClazz, clonedModule)
                  case _ => (clazz, module)
                }
                // currentMirror.mirrorDefining(cls) might side effect by entering symbols into pkgClass.info.decls
                // therefore, even though in the beginning of this method, super.lookupEntry(name) returned null
                // entering clazz/module now will result in a double-enter assertion in PackageScope.enter
                // here's how it might happen
                // 1) we are the rootMirror
                // 2) cls.getClassLoader is different from our classloader
                // 3) mirrorDefining(cls) looks up a mirror corresponding to that classloader and cannot find it
                // 4) mirrorDefining creates a new mirror
                // 5) that triggers Mirror.init() of the new mirror
                // 6) that triggers definitions.syntheticCoreClasses
                // 7) that might materialize symbols and enter them into our scope (because syntheticCoreClasses live in rootMirror)
                // 8) now we come back here and try to enter one of the now entered symbols => BAM!
                // therefore we use enterIfNew rather than just enter
                enterIfNew(resClazz)
                enterIfNew(resModule)
                if (tuv && clNameCon) {
//                  val test = super.lookupEntry(name)
//                  println(s"test: $test")
                  println(s"rootMirror.hashCode: ${rootMirror.hashCode}")
                  println(s"clonedClazz.enclosingRootClass == rootMirror.RootClass: ${resClazz.enclosingRootClass == rootMirror.RootClass}")
                  println(s"clonedClazz.owner.enclosingRootClass: ${resClazz.owner.enclosingRootClass == rootMirror.RootClass}")
                  println("*********************************")
                  println
                } else if (clNameCon) {
                  println("Exceptional situation...")
                }
                resSymbols
              }
            debugInfo(s"created $module/${module.moduleClass} in $pkgClass")
            if (namecon) {
              println("--- else-2 {")
              println(s"  loadingMirror.hashCode():  ${loadingMirror.hashCode()}")
              println(s"  currentMirror.hashCode():  ${currentMirror.hashCode()}")
              println("}")
            }
            lookupEntry(name)
          case none =>
            debugInfo("*** not found : "+path)
            negatives += name
            null
        }
      }
      if (namecon) {
        println(s"<=== final lookupEntry (${rootMirror.hashCode()}): $r")
        if (r != null && r.sym != null) {
          print("{")
          print(s"  r.sym.eRC == rM.RC: ${r.sym.enclosingRootClass == rootMirror.RootClass}")
          println("  }")
        }
        println
      }
      r
    }
  }

  /** Assert that packages have package scopes */
  override def validateClassInfo(tp: ClassInfoType) {
    assert(!tp.typeSymbol.isPackageClass || tp.decls.isInstanceOf[PackageScope])
  }

  override def newPackageScope(pkgClass: Symbol) = new PackageScope(pkgClass)

  override def scopeTransform(owner: Symbol)(op: => Scope): Scope =
    if (owner.isPackageClass) owner.info.decls else op
}
