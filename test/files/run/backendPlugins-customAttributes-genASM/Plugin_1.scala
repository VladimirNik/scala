package backendAttrs.genASM

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin}

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import analyzer._

  val name = "backendCustomAttrs"
  val description = "A sample plugin to test BackendPlugin pluginsCustomAttributes with GenASM"
  val components = Nil
  addBackendPlugin(BPlugin1)
  addBackendPlugin(BPlugin2)
  addBackendPlugin(BPlugin3)

  import scala.tools.asm.CustomAttr
  def isTestSymbol(sym: ClassSymbol) = sym.name.toString.contains("TestAttr")

  class BPluginTemplate(customAttrs: List[CustomAttr]) extends BackendPlugin {
    override def isActive(): Boolean = true
    override def pluginsCustomAttributes(sym: ClassSymbol): List[CustomAttr] =
      if (isTestSymbol(sym)) customAttrs else Nil
  }

  object BPlugin1 extends BPluginTemplate(List(new CustomAttr("CUSTOMATTR1", Array(0,1,1,0)), new CustomAttr("CUSTOMATTR2", Array(1,0,0,1))))
  object BPlugin2 extends BPluginTemplate(List(new CustomAttr("CUSTOMATTR3", Array(0,1,1,0)))) {
    override def isActive(): Boolean = false
  }
  object BPlugin3 extends BPluginTemplate(List(new CustomAttr("CUSTOMATTR4", Array(0,1,1,0))))
}