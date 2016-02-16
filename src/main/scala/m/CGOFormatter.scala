package m

import goparser._
import ast._
import FormatHelpers.{indent,indentText}

object CGOFormatter {

    def renderType(t: GoType): String = t match {
      case r: ReferencedType =>
        // TODO - handle foreign package
        r.pkg.map(p => p + "." + r.name) getOrElse r.name
      case SliceType(v) =>
        s"[]${renderType(v)}"
      case PointerType(contained) =>
        s"*${renderType(contained)}"
      case MapType(keys, values) =>
        s"map[${renderType(keys)}]${renderType(values)}"
      case IntegerType(bits, signed) =>
        (if (signed) "" else "u") + "int" + bits.map(_.toString).getOrElse("")
      case BooleanType =>
        "boolean"
      case FloatType(bits) =>
        s"float${bits}"
      case ComplexType(bits) =>
        s"complex${bits}"

    }


  def cgoTpeName(s: GoType, scope: Scope, naming: Map[ScopedType.Gen, String]): String =
    s match {
      case PointerType(contained) =>
        s"*C.${renderType(contained)}"
      case IntegerType(Some(8), signed) =>
        "C." + (if (signed) "" else "u") + "char"
      case IntegerType(Some(16), signed) =>
        "C." + (if (signed) "" else "u") + "short"
      case IntegerType(Some(32), signed) =>
        "C." + (if (signed) "" else "u") + "long"
      case IntegerType(Some(64), signed) =>
        "C." + (if (signed) "" else "u") + "longlong"
      case r: ReferencedType =>
        val resolved = scope.resolveType(r)
        naming.get(resolved).map { r => "C." + r }.getOrElse {
          cgoTpeName(resolved.tpe, scope, naming)
        }
      case BooleanType =>
        "C.uchar"
      case FloatType(32) =>
        "C.float"
      case FloatType(64) =>
        "C.double"

      case unsupported =>
        s"C.unsupported(${unsupported}) // LOL"
    }

  case class FromCFormatter(struct: ScopedType[StructType], naming: Map[ScopedType.Gen, String]) {

    def cTpeReader(cReadExpr: String, t: GoType): (Option[String], String) = t match {
      case r: ReferencedType =>
          val resolved = struct.scope.resolveType(r)
          naming.get(resolved).map { r => (None, s"cToGo_${r}(${cReadExpr})") }.getOrElse {
            (None, s"${renderType(r)}(${cReadExpr})")
          }

      case StringType =>
        (None, s"C.GoString(${cReadExpr})")
      case ArrayType(IntegerType(Some(8), false)) | SliceType(IntegerType(Some(8), false)) =>
        (None, s"C.GoBytes(${cReadExpr}, C.int(${cReadExpr}Len))")
      case PointerType(subType) =>
        val (preamble, getter) = cTpeReader(cReadExpr, subType)
        val ptrValue = newVarName("ptrValue")

        ( Some(List(preamble, Some(s"${ptrValue} := ${getter}")).flatten.mkString("\n")),
          s"&${ptrValue}" )

      case IntegerType(_, _) =>
        (None, s"${renderType(t)}(${cReadExpr})")
      case SliceType(valuesTpe) =>
        val lenVar = newVarName("len")
        val cgoCastedSliceName = newVarName("cgoSlice")
        val castedSliceName = newVarName("slice")
        val cgoSliceTypeName = cgoTpeName(valuesTpe, struct.scope, naming)
        val idxVar = newVarName("idx")
        val cgoItemVar = newVarName("cgoItem")
        val (castPreamble, castReadExpr) = cTpeReader(cgoItemVar, valuesTpe)
        (
          Some(
            s"""
            |${lenVar} := int(${cReadExpr}Len)
            |${cgoCastedSliceName} := (*[1 << 30]${cgoSliceTypeName})(unsafe.Pointer(${cReadExpr}))[:${lenVar}:${lenVar}]
            |${castedSliceName} := make([]${renderType(valuesTpe)}, ${lenVar})
            |for ${idxVar}, ${cgoItemVar} := range ${cgoCastedSliceName} {
            |  ${castPreamble.getOrElse("")}
            |  ${castedSliceName}[${idxVar}] = ${castReadExpr}
            |}
            |""".stripMargin),
          castedSliceName)
      case _ =>
        // // TODO - cast using GoType in case if alias was used
        // val resolved = struct.scope.resolveType(t)
        // ???
        (None, s"NotImplemented(${cReadExpr}) // ${t}")
    }
    private var idx = 0
    def newVarName(prefix: String = "_v") = {
      idx += 1
      prefix + idx
    }



    def structFieldReader(cStructName: String, fieldName: String, t: GoType): (Option[String], (String, String)) = {
      val cReadExpr = s"${cStructName}.${fieldName}"
      cTpeReader(cReadExpr, t) match {
        case (preamble, expr) =>
          (preamble, (fieldName, expr))
      }
    }

    lazy val name = naming(struct)

    override def toString = {
      val (preambleItems, fieldSeqExprs) = struct.tpe.fields.collect {
        case f: StructField =>
          structFieldReader("struct", f.name, f.tpe)
        case _ =>
          ???
      }.unzip

      val preamble = preambleItems.flatten.mkString("\n")

      val fieldsExpr = fieldSeqExprs.map { case (fieldName, readExpr) =>
        s"${fieldName}: ${readExpr}"
      }

      // TODO - need to add namespace / import for struct.name if rendering into foreign namespace
      val goStructExpr = s"""
      |${struct.name} {
      |${fieldsExpr.map(indent(2)).mkString(",\n")} }
      """.stripMargin.trim

      s"""
      |func cToGo_${name}(struct C.${name}) ${struct.name} {
      |${indentText(preamble, 2)}
      |  return ${indentText(goStructExpr, 2, false)}
      |}
      """.stripMargin
    }
  }
  
  case class StructFormatter(struct: ScopedType[StructType], naming: Map[ScopedType.Gen, String]) {
    private def signedKw(signed: Boolean) =
      (if (signed) "signed" else "unsigned")

    def formatTpe(t: GoType): String = t match {
      case IntegerType(Some(8), signed) =>
        s"${signedKw(signed)} char "
      case IntegerType(Some(16), signed) =>
        s"${signedKw(signed)} short "
      case IntegerType(Some(32), signed) =>
        s"${signedKw(signed)} long "
      case IntegerType(Some(64), signed) =>
        s"${signedKw(signed)} long long "
      case StringType =>
        s"char *"
      case r: ReferencedType =>
        val s = struct.scope.resolveType(r)
        naming.get(s) match {
          case Some(name) =>
            name + " "
          case None =>
            formatTpe(s.tpe)
        }
      case PointerType(t) =>
        // WARNING - ignoring!
        formatTpe(t)
      case BooleanType =>
        "unsigned char "
      case _ =>
        s"Unsupported($t)"
    }

    def formatField(name: String, t: GoType, pointers: String = ""): Seq[String] = {
      def o(formattedTpe:String) =
        List(s"${formattedTpe}${name}")

      t match {
        // byte array
        case ArrayType(IntegerType(Some(8), false)) | SliceType(IntegerType(Some(8), false)) if pointers == "" =>
          // TODO - how to do pointer to an array?
          List(
            s"void *${name}",
            s"long ${name}Len")

        case a: ListType if pointers == "" =>
          // TODO - how to handle??
          val valuesName = formatTpe(a.values)
          List(
            s"${valuesName}*${name}",
            s"long ${name}Len")

        case a: ListType if pointers != "" =>
          List("pointers to arrays is unsupported")

        case PointerType(tpe) =>
          // pointers are just ignored now
          formatField(name, tpe, pointers)
        case r: ReferencedType =>
          val s = struct.scope.resolveType(r)

          /* resolve here instead of formatTpe so we can inline type aliased
           * arrays
           */
          naming.get(s).
            map(alias => o(alias + " ")).
            getOrElse({
              formatField(name, s.tpe, pointers)
            })

        case _ =>
          o(formatTpe(t))
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

    lazy val name = naming(struct)

    override def toString = {
      (List(s"typedef struct ${name} {") :+
        struct.tpe.fields.flatMap(formatField).map(indent(2)).mkString(",\n") :+
        s"} ${name};").mkString("\n")
    }

  }
}
