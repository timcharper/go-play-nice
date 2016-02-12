package goparser

import fastparse.noApi._
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
    ("map[" ~/ maybePointer(sliceTpe | valueTpe) ~ "]" ~ tpe)
  } map {
    case (keyType, valueType) =>
      MapType(keyType, valueType)
  }

  def valueTpe: Parser[NamedType] = P {
    identifier.map(NamedType)
  }

  def tpe: Parser[GoType] = P {
    maybePointer(sliceTpe | mapTpe | valueTpe)
  }

  def spaceDelim = (inlineComment | " " | "\t").rep(1)
  def structField: Parser[StructField] = P {
    identifier ~ spaceDelim ~ tpe ~ spaceDelim ~ shortstring.? ~ spaceDelim.? ~ &("\n" | "}")
  }.map(StructField.tupled)

  def structFieldInclude: Parser[StructFieldInclude] = P {
    tpe ~ (spaceDelim ~ shortstring).? ~ spaceDelim.? ~ &("\n" | "}")
  }.map(StructFieldInclude.tupled)

  def structItem: Parser[StructItem] =
    structField | structFieldInclude

}


object WsApi extends fastparse.WhitespaceApi.Wrapper(Lexical.wscommentnl)

object GoParser {
  import Lexical.kw
  import WsApi._
  import Lexical._
  def pkg: Parser[Package] =
    P("package" ~ identifier).map(Package)

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
}
