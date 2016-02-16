package m
// import fastparse.core.Parsed
// import goparser._
import fastparse.core.ParseError
import fastparse.core.Parsed
import org.apache.commons.io.FileUtils
import java.io.File
import goparser._
import scala.annotation.tailrec

object Main extends App {
  import ast._



  val GOPATH = new File("./integration_test/single_file")
  val u = new ParseUniverse(GOPATH)
  val pkg = u.pkg(new File(GOPATH, "hancock"))
  println(pkg)
  println("===================")
  val scopedFn = pkg.getFunc("VerifyCertificateSignedMessage").get
  // val scopedFn = pkg.getType("PublicKey").get
  println(scopedFn.tpe)
  println("===================")

  val ents = u.allDependencies(List(scopedFn))
  val destPkg = u.pkg("hancockc")
  val named = destPkg.withNames(ents)
  named.foreach {
    case (scopedVal @ ScopedType(_, _, f: StructType), name) =>
      println()
      println(CGOFormatter.StructFormatter(ScopedType(scopedVal.scope, scopedVal.name, f), named).toString)
      println(CGOFormatter.FromCFormatter(ScopedType(scopedVal.scope, scopedVal.name, f), named).toString)
    case (s, name) =>
      println()
      println(s"${name}:")
      println(s"${s.tpe}")
      
  }

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
