package m

import goparser._
import ast._

object StructFormatter {
  private def signedKw(signed: Boolean) = 
    (if (signed) "signed" else "unsigned")
  def formatTpe(t: GoType): String = t match {
    case IntegerType(Some(8), signed) =>
      s"${signedKw(signed)} char"
    case IntegerType(Some(16), signed) =>
      s"${signedKw(signed)} short"
    case IntegerType(Some(32), signed) =>
      s"${signedKw(signed)} long"
    case IntegerType(Some(64), signed) =>
      s"${signedKw(signed)} long long"
    case _ =>
      s"NotImplemented(${t.toString})"
  }

  def formatField(name: String, t: GoType, pointers: String = ""): Seq[String] = {
    t match {
      // byte array
      case ArrayType(IntegerType(Some(8), false)) | SliceType(IntegerType(Some(8), false)) =>
        List(
          s"void ${pointers}*${name}",
          s"long ${pointers}${name}Len")
      case PointerType(tpe) =>
        formatField(name, tpe, pointers + "*")
      case _ =>
        List(s"${formatTpe(t)} ${pointers}${name}")
    }
  }

  def formatField(s: StructItem): Seq[String] = {
    s match {
      case StructField(key, tpe, _) =>
        formatField(key, tpe)
      case _ =>
        List("/* not supported */")
    }
  }

  def indent(n: Int): String => String =
    (" " * n) + (_ : String)

  def format(struct: StructDef) =
    struct match {
      case StructDef(name, fields) =>
        (List(s"typedef struct ${name} {") :+
          fields.flatMap(formatField).map(indent(2)).mkString(",\n") :+
          s"} ${name};").mkString("\n")
    }
}
