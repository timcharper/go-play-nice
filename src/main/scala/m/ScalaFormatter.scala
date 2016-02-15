package m

import goparser._
import ast._
import FormatHelpers._

object ScalaFormatter {
  def formatTpe(t: GoType, scope: Scope, naming: Map[ScopedType.Gen, String]): String = {
    t match {
      case a: ListType =>
        a.values match {
          case IntegerType(Some(8), _) =>
            s"Array[Byte]"
          case _ =>
            s"Seq[${formatTpe(a.values, scope, naming)}]"
        }

      case IntegerType(Some(8), _) =>
        s"Byte"
      case IntegerType(Some(16), _) =>
        s"Short"
      case IntegerType(Some(32), _) =>
        s"Int"
      case IntegerType(Some(64), _) =>
        s"Long"
      case StringType =>
        s"String"
      case r: ReferencedType =>
        val s = scope.resolveType(r)
        naming.get(s) match {
          case Some(name) =>
            name
          case None =>
            formatTpe(s.tpe, s.scope, naming)
        }
      case PointerType(subType) =>
        // WARNING - ignoring!
        formatTpe(subType, scope, naming)
      case BooleanType =>
        "Boolean"
      case _ =>
        s"Unsupported($t)"
    }
  }

  case class StructFormatter(struct: ScopedType[StructType], naming: Map[ScopedType.Gen, String]) {
    private def signedKw(signed: Boolean) =
      (if (signed) "signed" else "unsigned")

    def formatField(s: StructItem): String = {
      s match {
        case StructField(key, tpe, _) =>
          s"${key}: ${formatTpe(tpe, struct.scope, naming)}"
        case _ =>
          "/* not supported */"
      }
    }

    lazy val name = naming(struct)

    override def toString = {
      (List(s"case class ${name}(") :+
        struct.tpe.fields.map(formatField).map(indent(2)).mkString(",\n") :+
        s")").mkString("\n")
    }
  }

  case class FuncFormatter(func: ScopedType[FuncType], naming: Map[ScopedType.Gen, String]) {
    lazy val name = naming(func)

    def formatArg(arg: FuncArg): String = {
      s"${arg.name}: ${formatTpe(arg.tpe, func.scope, naming)}"
    }

    def formatResult(tpe: GoType): String = {
      s"${formatTpe(tpe, func.scope, naming)}"
    }

    override def toString = {
      (List(s"def ${name}(") :+
        func.tpe.args.map(formatArg).map(indent(2)).mkString(",\n") :+
        "): (" :+
        func.tpe.results.map(formatResult).map(indent(2)).mkString(",\n") :+
        ")").mkString("\n")
    }
  }

}
