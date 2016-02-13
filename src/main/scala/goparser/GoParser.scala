package goparser

import fastparse.noApi._
import ast._
import scala.annotation.tailrec
// import Expressions._

object Basic {
  val Newline = P( StringIn("\r\n", "\n") )
}

object Lexical {
  import fastparse.all._
  def kw(s: String) = s ~ !(letter | digit | "_")
  val letter     = P( lowercase | uppercase )
  val lowercase  = P( CharIn('a' to 'z') )
  val uppercase  = P( CharIn('A' to 'Z') )
  val digit      = P( CharIn('0' to '9') )

  val identifier: P[String] =
    P( (letter|"_") ~ (letter | digit | "_").rep ).!.filter(!keywordList.contains(_))

  val keyword = P { StringIn(keywordList.toSeq : _*) }
  val keywordList = Set(
    "break", "default", "func", "interface", "select",
    "case", "defer", "go", "map", "struct",
    "chan", "else", "goto", "package", "switch",
    "const", "fallthrough", "if", "range", "type",
    "continue", "for", "import", "return", "var")

  // Comments cannot have cuts in them, because they appear before every
  // terminal node. That means that a comment before any terminal will
  // prevent any backtracking from working, which is not what we want!
  val commentChunk = P( CharsWhile(!"/*".contains(_)) | multilineComment | !"*/" ~ AnyChar )
  val multilineComment: P0 = P( "/*" ~/ commentChunk.rep ~ "*/" )

  val inlineCommentChunk = P( CharsWhile(!"/*".contains(_)) | !"*/" ~ AnyChar )
  val inlineComment: P0 = P( "/*" ~/ inlineCommentChunk.rep ~ "*/")
  val sameLineCharChunks = P( CharsWhile(!"\n\r".contains(_))  | !Basic.Newline ~ AnyChar )
  val lineComment = P( "//" ~ sameLineCharChunks.rep ~ &(Basic.Newline | End) )
  val comment: P0 = P( multilineComment | lineComment )

  val wscommentnl = P( (CharsWhile(" \n".toSet, min = 1) | lineComment | "\\\n").rep )

  val escapeseq: P0 = P( "\\" ~ AnyChar )
  def shortstring = shortstring0("\"") | shortstring0("`")
  def shortstring0(delimiter: String) = P( delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
  def shortstringitem(quote: String): P0 = P( shortstringchar(quote) | escapeseq )
  def shortstringchar(quote: String): P0 = P( CharsWhile(!s"\\\n${quote(0)}".contains(_)) )

  def maybePointer(p: Parser[GoType]): Parser[GoType] = P {
    ("*" ~/ p).map(PointerType) | p
  }

  def sliceTpe: Parser[SliceType] = P {
    (("[" ~/ digit.rep(0).! ~ "]").rep(1) ~ tpe)
  } map {
    case (sizeChars, values) =>
      val sizeStr = sizeChars.mkString
      val size = if (sizeStr.length == 0)
        None
      else {
        Some(sizeStr.mkString("").toInt)
      }

      SliceType(size, values)
  }

  def mapTpe: Parser[MapType] = P {
    ("map[" ~/ maybePointer(sliceTpe | primitiveTpe | referencedTpe) ~ "]" ~ tpe)
  } map {
    case (keyType, valueType) =>
      MapType(keyType, valueType)
  }

  def stringTpe: Parser[StringType.type] = P {
    "string".!.map { _ => StringType }
  }

  def referencedTpe: Parser[ReferencedType] = P {
    ((identifier ~ ".").? ~ identifier).map(ReferencedType.tupled)
  }

  def tpe: Parser[GoType] = P {
    maybePointer(sliceTpe | mapTpe | primitiveTpe | referencedTpe)
  }

  def spaceDelim = (inlineComment | " " | "\t").rep(1)
  def structField: Parser[StructField] = P {
    identifier ~ spaceDelim ~ tpe ~ spaceDelim ~ shortstring.? ~ spaceDelim.? ~ &("\n" | "}")
  }.map(StructField.tupled)

  def structFieldInclude: Parser[StructFieldInclude] = P {
    tpe ~ (spaceDelim ~ shortstring).? ~ spaceDelim.? ~ &("\n" | "}")
  }.map(StructFieldInclude.tupled)

  def complexTpe: Parser[ComplexType] = P {
    ("complex" ~ ("64" | "128").!).map { bits =>
      ComplexType(bits.toInt)
    }
  }
  def floatTpe: Parser[FloatType] = P {
    ("float" ~ ("32" | "64").!).map { bits =>
      FloatType(bits.toInt)
    }
  }

  def byteTpe: Parser[IntegerType] = P {
    "byte".!.map { _ =>
      IntegerType(Some(8), false)
    }
  }

  def integerTpe: Parser[IntegerType] = byteTpe | P {
    ("u".!.? ~ "int" ~ ("8" | "16" | "32" | "64").!.?) map {
      case (unsigned, bits) =>
        IntegerType(bits.map(_.toInt), unsigned.isEmpty)
    }
  }

  def boolTpe: Parser[BooleanType.type] = P {
    "boolean".!.map { _ =>
      BooleanType
    }
  }

  def structItem: Parser[StructItem] =
    structField | structFieldInclude

  def primitiveTpe =
    integerTpe | floatTpe | complexTpe | boolTpe | stringTpe

}


object WsApi extends fastparse.WhitespaceApi.Wrapper(Lexical.wscommentnl)

object GoParser {
  import Lexical.kw
  import WsApi._
  import Lexical._
  def pkg: Parser[PackageDef] =
    P("package" ~ identifier).map(PackageDef)

  def imports: Parser[Seq[Import]] =
    importSingle | importMultiple

  def importPair: Parser[Import] = P { identifier.? ~ shortstring }.map {
    case (alias, singleImport) =>
      Import(alias, singleImport)
  }

  def importMultiple: Parser[Seq[Import]] =
    "import" ~ "(" ~ importPair.rep(1)  ~ ")"

  def importSingle: Parser[Seq[Import]] =
    ("import" ~ importPair).map(List(_))

  def structFieldInclude: Parser[StructFieldInclude] = P {
    tpe ~ shortstring.? ~ &("\n" | "}")
  }.map(StructFieldInclude.tupled)

  def struct: Parser[StructDef] = P {
    "type" ~ identifier ~ "struct" ~ "{" ~ structItem.rep(0) ~ "}"
  }.map {
    case (name, fields) => StructDef(name, fields.toList)
  }

  def namedFunctionDef: Parser[NamedFunctionDef] = P {
    def inParens[T](p: Parser[T]): Parser[T] = "(" ~ p ~ ")"
    val contextArg: Parser[GoType] = inParens(
      identifier ~ tpe
    ).map {
      case (_, t) =>
        t
    }

    val functionArgTup: Parser[(String, Option[GoType])] = (
      identifier ~ tpe.?
    )

    val returnArg: Parser[GoType] = (
      identifier.? ~ tpe
    ).map {
      case (_, t) =>
        t
    }

    val returnArgs: Parser[Seq[GoType]] = (returnArg.rep(min=0, max=1) |
      inParens(returnArg.rep(min = 0, sep = P{","})))

    @tailrec
    def resolveArgTypes(
      reverseArgs: List[(String, Option[GoType])],
      acc: List[FunctionArg] = List.empty,
      lastArgType: GoType = InterfaceType()): List[FunctionArg] = {
      reverseArgs match {
        case Nil =>
          acc
        case (name, argTpe) :: rest =>
          val t = argTpe.getOrElse(lastArgType)
          resolveArgTypes(
            rest,
            FunctionArg(name, t) :: acc,
            t)
      }
    }

    val functionArgs: Parser[Seq[FunctionArg]] = {
      inParens(functionArgTup.rep(min = 0, sep = P{","})).map { l =>
        resolveArgTypes(l.reverse.toList)
      }
    }


    val functionDefHeader = ("func" ~ contextArg.? ~ identifier ~
      functionArgs ~
      returnArgs).map(NamedFunctionDef.tupled)

    functionDefHeader ~ block
  }


  def parenExpr: Parser[Unit] = P {
    "(" ~ contents.rep ~ ")"
  }

  def block: Parser[Unit] = P {
    "{" ~ contents.rep ~ "}"
  }

  def contents = P {
    val charThings = Set('/', '(', ')', '{', '}', '"', '`')
    (CharsWhile(c => ! (charThings contains c)) | shortstring | block | comment | parenExpr).map { _ => () }
  }

}
