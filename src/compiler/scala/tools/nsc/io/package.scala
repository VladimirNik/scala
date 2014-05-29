/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc

import scala.language.implicitConversions
import scala.reflect.internal.tools.nsc.{io => rio}

package object io {
  // Forwarders from scala.reflect.io
  type AbstractFile = rio.AbstractFile
  val AbstractFile = rio.AbstractFile
  type Directory = rio.Directory
  val Directory = rio.Directory
  type File = rio.File
  val File = rio.File
  type Path = rio.Path
  val Path = rio.Path
  type PlainFile = rio.PlainFile
  val Streamable = rio.Streamable
  type VirtualDirectory = rio.VirtualDirectory
  type VirtualFile = rio.VirtualFile
  type ZipArchive = rio.ZipArchive

  type JManifest = rio.JManifest
  type JFile = rio.JFile

  import scala.reflect.internal.tools.nsc.io.Jar
  implicit def enrichManifest(m: JManifest): Jar.WManifest = rio.enrichManifest(m)
}
