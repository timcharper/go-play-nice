package m
// import fastparse.core.Parsed
// import goparser._
import fastparse.core.ParseError
import fastparse.core.Parsed
import java.io.FileWriter
import java.io.PrintWriter
import org.apache.commons.io.FileUtils
import java.io.File
import goparser._
import scala.annotation.tailrec

object Main extends App {
  import ast._



  val GOPATH = new File(System.getenv("GOPATH"))
  val u = new ParseUniverse(GOPATH)
  val pkg = u.pkg("gofunc")
  println(pkg)
  println("===================")
  val scopedFn = pkg.getFunc("FormatPerson").get
  println(scopedFn.tpe)
  println("===================")

  val ents = u.allDependencies(List(scopedFn))
  val destPkg = u.pkg("hancockc")
  val named = destPkg.withNames(ents)
  val imports = destPkg.generateImports(ents)

  val headerFile = new PrintWriter(new File(destPkg.path, "module_types.h"))
  val goFile = new PrintWriter(new File(destPkg.path, "module.go"))
  goFile.println(s"package ${destPkg.name}")
  goFile.println("import (")
  val terminalSlash = "/$".r
  for ((path, Some(alias)) <- imports.toSeq if alias.nonEmpty) {
    val relative = terminalSlash.replaceAllIn(
      u.goSrcPath.toURI.relativize(path.toURI).getPath,
      "")
    goFile.println(s"""  ${alias} "${relative}"""")
  }
  goFile.println(")")
  goFile.println(s"""
  |/*
  |#include <stdlib.h>
  |#include <stdint.h>
  |#include "module_types.h"
  |*/
  |import "C"
  |import "unsafe"
  |""".stripMargin)


  named.foreach {
    case (scopedVal @ ScopedType(_, _, f: StructType), name) =>
      println()
      headerFile.println(CGOFormatter.StructFormatter(ScopedType(scopedVal.scope, scopedVal.name, f), named).toString)
      headerFile.println()
      goFile.println(CGOFormatter.CToGoFormatter(ScopedType(scopedVal.scope, scopedVal.name, f), named, imports).toString)
      goFile.println()
    case (s, name) =>
      println()
      headerFile.println(s"/* ${name}: \n ${s.tpe} */")
  }

  headerFile.close
  goFile.close
  println("==================================================================")

  named.foreach {
    case (scopedVal @ ScopedType(_, _, f: StructType), name) =>
      println()
      println(ScalaFormatter.StructFormatter(ScopedType(scopedVal.scope, scopedVal.name, f), named).toString)
    case (scopedVal @ ScopedType(_, _, f: FuncType), name) =>
      println()
      println(ScalaFormatter.FuncFormatter(ScopedType(scopedVal.scope, scopedVal.name, f), named).toString)
    case (s, name) =>
      println()
      println(s"${name}:")
      println(s"${s.tpe}")
      
  }
}
