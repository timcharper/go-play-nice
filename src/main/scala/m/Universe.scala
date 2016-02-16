package m

import fastparse.core.ParseError
import fastparse.core.Parsed
import org.apache.commons.io.FileUtils
import java.io.File
import goparser._
import scala.annotation.tailrec
import ast._

case class ScopedType[+T <: GoType](scope: Scope, name: String, tpe: T) {
  def typeDependencies =
    scope.typeDependencies(tpe)
}
object ScopedType {
  type Gen = ScopedType[GoType]
}

case class Scope(pkg: GoPackage, goFile: GoFile) {
  def resolveTypeOnce(tpe: ReferencedType): ScopedType.Gen = {
    {
      tpe match {
        case ReferencedType(Some(alias), name) =>
          pkg.universe.pkg(goFile.imports(alias)).getType(name)
        case ReferencedType(None, name) =>
          pkg.getType(name)
      }
    } getOrElse {
      throw new RuntimeException(s"Could not resolve type ${tpe} in ${pkg.path}")
    }
  }

  val resolveType: SimpleCache[ReferencedType, ScopedType.Gen] = SimpleCache[ReferencedType, ScopedType.Gen] { tpe =>
    resolveTypeOnce(tpe) match {
      case ScopedType(scope, _, t: ReferencedType) =>
        scope.resolveType(t)
      case otherwise =>
        otherwise
    }
  }

  def isPrivate(n: String) = {
    n.head.toLower == n.head
  }

  // Immediate list of all referenced types
  def typeDependencies(tpe: GoType): List[ScopedType.Gen] = tpe match {
    case t: ReferencedType =>
      resolveType(t) match {
        case ScopedType(_, _, p: GoPrimitive) =>
          // println(s"${tpe} is a primitive, ${p}")
          Nil
        case other if isPrivate(other.name) =>
          ???
        case other =>
          List(other)
      }
    case i: InterfaceType =>
      ???
    case StructType(fields) =>
      fields.collect {
        case StructField(_, fieldTpe, _) =>
          typeDependencies(fieldTpe)
        case t: StructInclude =>
          ???
      }.flatten.toList
    case SliceType(valuesTpe) =>
      typeDependencies(valuesTpe)
    case ArrayType(valuesTpe) =>
      typeDependencies(valuesTpe)
    case MapType(keysTpe, valuesTpe) =>
      (typeDependencies(keysTpe) ++
        typeDependencies(valuesTpe)).distinct
    case FuncType(args, results) =>
      (args.map(_.tpe) ++ results).
        map(typeDependencies).
        flatten.
        toList.
        distinct
    case PointerType(tpe) =>
      typeDependencies(tpe)
    case t: GoPrimitive =>
      Nil
  }
}

case class GoPackage(universe: ParseUniverse, path: File, members: Seq[GoFile]) {
  lazy val name = path.getName

  def getFunc(name: String): Option[ScopedType[FuncType]] = {
    members.
      find(_.funcs.contains(name)).
      map { goFile => ScopedType(Scope(this, goFile), name, goFile.funcs(name)) }
  }

  def getType(name: String): Option[ScopedType.Gen] = {
    members.
      find(_.typeDefs.contains(name)).
      map { goFile => ScopedType(Scope(this, goFile), name, goFile.typeDefs(name)) }
  }

  private def suffixes(pkgName: String) = (Stream("", pkgName.capitalize) ++ Stream.from(1).map(_.toString))
  @tailrec
  final def withNames(remaining: List[ScopedType.Gen],
    usedNames: Set[String] = Set.empty, // TODO - seed with reserved words!!!
    names: Map[ScopedType.Gen, String] = Map.empty): Map[ScopedType.Gen, String] = remaining match {
    case Nil =>
      names
    case head :: rest =>
      val suffix = suffixes(head.scope.pkg.name).find { s => !(usedNames contains (head.name + s)) }.get
      val suffixedName = head.name + suffix
      withNames(rest, usedNames + suffixedName, names.updated(head, suffixedName))
  }
}

case class ParseUniverse(goPath: File) {
  private val pkgCache = SimpleCache[File, GoPackage] { cf =>
    val members = Option(cf.listFiles).getOrElse(Array()).filter { f =>
      val filename = f.getName.toLowerCase
      filename.endsWith(".go") && ! filename.endsWith("_test.go")
    }.map { f =>
      println(f)
      val contents = FileUtils.readFileToString(f)
      GoProgram.program.parse(contents) match {
        case Parsed.Success(m, _) =>
          m
        case failure: Parsed.Failure =>
          throw new RuntimeException(s"Error while parsing ${f}", ParseError(failure))
      }
    }.toList
    GoPackage(this, cf, members)
  }

  def pkg(f: File): GoPackage =
    pkgCache(f.getCanonicalFile)

  def pkg(s: String): GoPackage =
    pkg(new File(goPath, s))

  @tailrec
  final def allDependencies(scopedTypes: List[ScopedType.Gen], visited: List[ScopedType.Gen] = List.empty): List[ScopedType.Gen] = scopedTypes match {
    case Nil =>
      visited
    case head :: rest if visited contains head =>
      allDependencies(rest, visited)
    case head :: rest =>
      allDependencies(head.typeDependencies ++ scopedTypes, head :: visited)
  }

}
