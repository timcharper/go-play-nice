package goparser
import ast._

import scala.annotation.tailrec

object Lexical {
  import fastparse.all._
  val Newline = P( StringIn("\r\n", "\n") )
  val space = CharIn(Seq(' ', '\t')).rep(1)
  val letter     = P( lowercase | uppercase )
  val lowercase  = P( CharIn('a' to 'z') )
  val uppercase  = P( CharIn('A' to 'Z') )
  val decimal_digit      = P( CharIn('0' to '9') )
  val exponent = (CharIn(Seq('e', 'E')) ~ CharIn(Seq('-', '+')).? ~ decimal_digit.rep)
  val decimal_lit = CharIn('1' to '9') ~ decimal_digit.rep
  val decimals = decimal_digit ~ decimal_digit.rep
  // val float_lit = digit.rep ~ "." ~ digit.rep
  val float_lit = P {
    (decimals ~ "." ~ decimals.? ~ exponent.?) |
      (decimals ~ exponent) |
      ("." ~ decimals ~ exponent.?)
  }.!

  val imaginary_lit = P((decimals | float_lit) ~ "i").!

  val rune_lit = shortstring0("'") // punt!
  private val hexDigit = CharIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
  val hex_lit = "0" ~ CharIn(Seq('x', 'X')) ~ hexDigit
  val int_lit: Parser[String] = (decimal_lit /*| octal_lit*/ | hex_lit).!
  // val operator = P { CharIn(Seq('+', '-', '*', '/')) | StringIn("==", "<<", ">>") }

  val rel_op     = StringIn("==", "!=", "<", "<=", ">", ">=")
  val add_op     = CharIn(Seq('+', '-', '|', '^'))
  val mul_op     = StringIn("*", "/", "%", "<<", ">>", "&", "&^")
  val binary_op  = StringIn("||", "&&") | rel_op | add_op | mul_op
  val unary_op   = StringIn("+", "-", "!", "^", "*", "&", "<-")

  val word: P[String] =
    P( (letter|"_") ~ (letter | decimal_digit | "_").rep ).!

  val unicode = P { CharsWhile(_.toInt > 255) }
  val identifier: P[String] = P {
    (letter ~ ( letter | decimal_digit | unicode ).rep).!
  }

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
  val sameLineCharChunks = P( CharsWhile(!"\n\r".contains(_))  | !Newline ~ AnyChar )
  val lineComment = P( "//" ~ sameLineCharChunks.rep ~ &(Newline | End) )
  val comment: P0 = P( multilineComment | lineComment )

  val wscomment: P0 = P { (space | lineComment | inlineComment).rep }
  val lineDelimiter : P0 = P { (Newline | multilineComment).rep(1) }

  val escapeseq: P0 = P( "\\" ~ AnyChar )
  def string_lit = shortstring0("\"") | shortstring0("`")
  def shortstring0(delimiter: String) = P( delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
  def shortstringitem(quote: String): P0 = P( shortstringchar(quote) | escapeseq )
  def shortstringchar(quote: String): P0 = P( CharsWhile(!s"\\\n${quote(0)}".contains(_)) )

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

  def stringTpe: Parser[StringType.type] = P {
    "string".!.map { _ => StringType }
  }

}
