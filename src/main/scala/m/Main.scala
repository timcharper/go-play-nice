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


  @tailrec
  def allDependencies(scopedTypes: List[ScopedType.Gen], visited: List[ScopedType.Gen] = List.empty): List[ScopedType.Gen] = scopedTypes match {
    case Nil =>
      visited
    case head :: rest if visited contains head =>
      allDependencies(rest, visited)
    case head :: rest =>
      allDependencies(head.typeDependencies ++ scopedTypes, head :: visited)
  }

  val suffixes = (Stream("") ++ Stream.from(1).map(_.toString))
  @tailrec
  def withNames(remaining: List[ScopedType.Gen],
    primaryPath: File, // Note - canonicalize!!!
    usedNames: Set[String] = Set.empty, // TODO - seed with reserved words!!!
    names: Map[ScopedType.Gen, String] = Map.empty): Map[ScopedType.Gen, String] = remaining match {
    case Nil =>
      names
    case head :: rest =>
      val pickedName = if (head.scope.pkg.path == primaryPath)
        head.name
      else
        head.scope.pkg.name + "__" + head.name

      val suffix = suffixes.find { s => !(usedNames contains (pickedName + s)) }.get
      val suffixedName = pickedName + suffix
      withNames(rest, primaryPath, usedNames + suffixedName, names.updated(head, suffixedName))
  }

  val ents = allDependencies(List(scopedFn))
  val named = withNames(ents, scopedFn.scope.pkg.path)
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
