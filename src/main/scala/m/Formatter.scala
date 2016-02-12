package m

import goparser._

object StructFormatter {
  def formatTpe(name: String, t: GoType, pointers: String = ""): Seq[String] = {
    t match {
      case SliceType(_, NamedType("byte")) =>
        List(
          s"void ${pointers}*${name}",
          s"long ${pointers}${name}Len")
      case NamedType("string") =>
        List(s"char *${pointers}${name}")
      case NamedType(nme) =>
        List(s"${nme} ${pointers}${name}")
      case PointerType(tpe) =>
        formatTpe(name, tpe, pointers + "*")
      case _ =>
        List(s"/* not implemented: ${name} ${pointers}${t} */")
    }

  }

  def formatField(s: StructItem): Seq[String] = {
    s match {
      case StructField(key, tpe, _) =>
        formatTpe(key, tpe)
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
