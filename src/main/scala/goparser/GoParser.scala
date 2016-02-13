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
  val space = CharIn(Seq(' ', '\t'))
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
  val multilineComment: P0 = P( "/*" ~ commentChunk.rep ~ "*/" )

  val inlineCommentChunk = P( CharsWhile(!"/*".contains(_)) | !"*/" ~ AnyChar )
  val inlineComment: P0 = P( "/*" ~ inlineCommentChunk.rep ~ "*/")
  val sameLineCharChunks = P( CharsWhile(!"\n\r".contains(_))  | !Basic.Newline ~ AnyChar )
  val lineComment = P( "//" ~ sameLineCharChunks.rep ~ &(Basic.Newline | End) )
  val comment: P0 = P( multilineComment | lineComment )

  val wscomment: Parser[Unit] = P { (space | lineComment | inlineComment).rep }
  val lineDelimiter : Parser[Unit] = P { (Basic.Newline | multilineComment).rep(1) }


  val escapeseq: P0 = P( "\\" ~ AnyChar )
  def shortstring = shortstring0("\"") | shortstring0("`")
  def shortstring0(delimiter: String) = P( delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
  def shortstringitem(quote: String): P0 = P( shortstringchar(quote) | escapeseq )
  def shortstringchar(quote: String): P0 = P( CharsWhile(!s"\\\n${quote(0)}".contains(_)) )

}


object WsApi extends fastparse.WhitespaceApi.Wrapper(Lexical.wscomment)

object GoTypes {
  import Lexical.kw
  import WsApi._
  import Lexical._

  def maybePointer(p: Parser[GoType]): Parser[GoType] = P {
    ("*" ~ p).map(PointerType) | p
  }

  def sliceTpe: Parser[SliceType] = P {
    (("[" ~ digit.rep(0).! ~ "]").rep(1) ~ tpe)
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
    ("map[" ~ maybePointer(sliceTpe | primitiveTpe | referencedTpe) ~ "]" ~ tpe)
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

  // TODO - move these below
  def structField: Parser[StructField] = P {
    identifier ~ tpe ~ shortstring.? ~ &(lineDelimiter | "}")
  }.map(StructField.tupled)

  // def structFieldInclude: Parser[StructFieldInclude] = P {
  //   tpe ~ (spaceDelim ~ shortstring).? ~ spaceDelim.? ~ &(lineDelimiter | "}")
  // }.map(StructFieldInclude.tupled)

  def structItem: Parser[StructItem] =
    structField | structFieldInclude

  def primitiveTpe =
    integerTpe | floatTpe | complexTpe | boolTpe | stringTpe | funcTpe

  def structFieldInclude: Parser[StructFieldInclude] = P {
    tpe ~ shortstring.? ~ &("\n" | "}")
  }.map(StructFieldInclude.tupled)

  def struct: Parser[StructDef] = P {
    "type" ~ identifier ~ "struct" ~/ "{" ~ lineDelimiter.rep ~
      (structItem ~ lineDelimiter.rep).rep ~
    "}"
  }.map {
    case (name, fields) => StructDef(name, fields.toList)
  }

  /* first item is a name, type is optional except for in last position; consume
   * parens. ie (a, b int) */
  val namedArgs: Parser[Seq[FuncArg]] = P {
    @tailrec
    def resolveArgTypes(
      reverseArgs: List[(String, Option[GoType])],
      acc: List[FuncArg],
      lastArgType: GoType): List[FuncArg] = {
      reverseArgs match {
        case Nil =>
          acc
        case (name, argTpe) :: rest =>
          val t = argTpe.getOrElse(lastArgType)
          resolveArgTypes(
            rest,
            FuncArg(name, t) :: acc,
            t)
      }
    }

    inParens {
      ((identifier ~ tpe.? ~ "," ~ lineDelimiter.rep).rep(min = 0) ~
        (identifier ~ tpe)).map {
        case (maybeMissing, (lastName, lastTpe)) =>
          resolveArgTypes(
            reverseArgs = (lastName, Some(lastTpe)) :: maybeMissing.reverse.toList,
            acc = List.empty,
            lastArgType = lastTpe)
      }
    }
  }

  /* series of types. no names. consume parens. ie (int64, error) */
  val unnamedArgs = P {
    inParens {
      tpe.rep(sep = ("," ~ lineDelimiter.rep))
    }
  }

  def inParens[T](p: Parser[T]): Parser[T] = "(" ~ lineDelimiter.rep ~ p ~ ")"

  val funcResultArgs: Parser[Seq[GoType]] =
    tpe.rep(min = 0, max = 1) |
      namedArgs.map(_.map(_.tpe)) |
      unnamedArgs

  // Go, for reasons, allows you to declare a func with parameters that have no name
  val positionallyNamedArgs =
    unnamedArgs.map(_.zipWithIndex.map {
      case (t, i) => FuncArg(s"arg${i.toString}", t)})

  def funcTypeArgs: Parser[Seq[FuncArg]] = P {
    namedArgs | positionallyNamedArgs
  }

  def funcTpe: Parser[FuncType] = P {
    ("func" ~ funcTypeArgs ~ funcResultArgs) map {
      case (args, resultArgs) =>
        FuncType(args, resultArgs)
    }
  }

}


object GoParser {
  import Lexical.kw
  import WsApi._
  import Lexical._
  import GoTypes.tpe
  def pkg: Parser[PackageDef] =
    P("package" ~ identifier).map(PackageDef)

  def imports: Parser[Seq[Import]] =
    importSingle | importMultiple

  def importPair: Parser[Import] = P { identifier.? ~ shortstring }.map {
    case (alias, singleImport) =>
      Import(alias, singleImport)
  }

  def importMultiple: Parser[Seq[Import]] = {
    "import" ~ "(" ~ lineDelimiter.rep ~ (importPair ~ lineDelimiter.rep(1)).rep(1) ~
    ")"
  }

  def spaceDelim = (inlineComment | " " | "\t").rep(1)


  def importSingle: Parser[Seq[Import]] =
    ("import" ~ importPair).map(List(_))


  def namedFuncDef: Parser[NamedFuncDef] = P {
    val contextArg: Parser[GoType] = GoTypes.inParens(
      identifier ~ tpe
    ).map {
      case (_, t) =>
        t
    }

    val resultArg: Parser[GoType] = (
      identifier.? ~ tpe
    ).map {
      case (_, t) =>
        t
    }

    val funcDefHeader = ("func" ~ contextArg.? ~ identifier ~/
      (GoTypes.namedArgs | GoTypes.positionallyNamedArgs) ~
      GoTypes.funcResultArgs).map(NamedFuncDef.tupled)

    funcDefHeader ~ block
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

  def interfaceMember: Parser[Either[(String, FuncType), Nothing]] = {
    (identifier ~ GoTypes.funcTypeArgs ~ GoTypes.funcResultArgs).map {
      case (name, args, resultArgs) =>
        Left((name, FuncType(args, resultArgs)))
    }
  }

  def interfaceInclude: Parser[Either[Nothing, ReferencedType]] =
    GoTypes.referencedTpe.map(Right(_))

  def interfaceItem: Parser[Either[(String, FuncType), ReferencedType]] = P {
    interfaceMember | interfaceInclude
  }

  def interfaceDef: Parser[InterfaceDef] = P {
    "type" ~ identifier ~ "interface" ~/ "{" ~ lineDelimiter.rep ~
      (interfaceItem).rep(sep = lineDelimiter) ~ lineDelimiter.? ~
    "}"
  }.map {
    case (name, fields) =>
      val members = fields.collect { case Left(v) => v }.toMap
      val includes = fields.collect { case Right(i) => i }.toList
      InterfaceDef(name, members, includes)
  }

}
