package goparser

import fastparse.noApi._
import ast._
import scala.annotation.tailrec
// import Expressions._

object Basic {
  val Newline = P( StringIn("\r\n", "\n") )
}


object WsApi extends fastparse.WhitespaceApi.Wrapper(Lexical.wscomment)

// object GoProgram {
//   import Lexical.kw
//   import WsApi._
//   import Lexical._
//   import GoTypes.tpe
// }

object GoParser {
  import WsApi._
  import Lexical._
  import GoTypes._

  def pkg: Parser[PackageDef] =
    P("package" ~ identifier).map(PackageDef)

  val Operand: Parser[Unit] = P {
    Literal | OperandName | MethodExpr | "(" ~ lineDelimiter.rep ~ Expression ~ ")"
  }

  val MethodExpr: Parser[Unit] = P {
    (ReceiverType ~ "." ~ lineDelimiter.rep ~ MethodName)
  }.map(_ => ())

  val ReceiverType = identifier
  val MethodName = identifier

  val Literal: Parser[Unit] = P {
    BasicLit.map(_ => ()) | CompositeLit | FunctionLit
  }

  val CompositeLit: Parser[Unit] = P { LiteralType ~ LiteralValue }
  val LiteralType: Parser[Unit] = P {
    structTpe | arrayTpe | ("[" ~ lineDelimiter.rep ~ "..." ~ "]" ~ tpe) |
    sliceTpe | mapTpe | referencedTpe
  }.map { _ => () }

  val structTpe: Parser[Unit] = P {
    "struct" ~ "{" ~ lineDelimiter.rep ~ (structItem ~ lineDelimiter.rep).rep ~ "}"
  }.map(_ => ())

  val LiteralValue: Parser[Unit] = P {
    "{" ~ lineDelimiter.rep ~ ElementList ~ ",".? ~ "}"
  }

  val ElementList: Parser[Unit] = P {
    Element.rep(sep = ",")
  }

  val FunctionLit: Parser[Unit] = P {
    funcTpe ~ block
  }.map(_ => ())

  def structDef: Parser[StructDef] = P {
    "type" ~ identifier ~ "struct" ~/ "{" ~ lineDelimiter.rep ~
      (structItem ~ lineDelimiter.rep).rep ~
    "}"
  }.map {
    case (name, fields) => StructDef(name, fields.toList)
  }

  val Element: Parser[Unit] = P { ( Key ~ ":" ).? ~ Value }
  val Key: Parser[Unit] = P {  FieldName | Expression | LiteralValue }
  val FieldName: Parser[Unit] = P {  identifier }.map(_ => ())
  val Value: Parser[Unit] = P {  Expression | LiteralValue }

  val BasicLit: Parser[String] = P {
    int_lit | float_lit | imaginary_lit | rune_lit | string_lit
  }

  val OperandName: Parser[Unit] = P {
    (identifier | QualifiedIdent).map(_ => ())
  }

  val PackageName = P { identifier }
  val QualifiedIdent = PackageName ~ "." ~ identifier

  def Expression: Parser[Unit] = P {
    UnaryExpr ~ (binary_op ~ Expression).rep
  }

  val UnaryExpr: Parser[Unit]  = P {
    PrimaryExpr | (unary_op ~ UnaryExpr)
  }

  val Conversion: Parser[Unit] = P {
    tpe ~ "(" ~ Expression ~ ",".? ~ ")"
  }.map ( _ => () )

  val PrimaryExpr: Parser[Unit] = P {
	  ( Operand |
	    Conversion) ~
	  (Selector | Index | Slice | TypeAssertion | Arguments).rep
  }

  val Selector: Parser[Unit] = P { ("." ~ identifier).map { _ => () } }
  val Index: Parser[Unit] = P { "[" ~ lineDelimiter.rep ~ Expression ~ "]" }
  val Slice: Parser[Unit] = P {
    "[" ~ (
      ( Expression.? ~ ":" ~ Expression.? ) |
      ( Expression.? ~ ":" ~ Expression ~ ":" ~ Expression )) ~
    "]"
  }

  val TypeAssertion: Parser[Unit] = P { "." ~ "(" ~ tpe ~ ")" }.map{ _ => () }
  val Arguments: Parser[Unit] = P {
    "(" ~ ( (ExpressionList | (tpe ~ ( "," ~ ExpressionList ).?) ) ~ "...".? ~ ",".?).? ~ ")"
  }.map { _ => () }

  val statementDelim = lineDelimiter.rep(1) | ";"
  val ConstDecl: Parser[Unit] = P {
    "const" ~ ( ConstSpec | (
      "(" ~ lineDelimiter.rep(1) ~ (ConstSpec.rep(sep = statementDelim)) ~ ")"
    ))
  }.map { _ => () }

  val ConstSpec      = P { IdentifierList ~ tpe.? ~ "=" ~ ExpressionList }

  val IdentifierList: Parser[Unit] = identifier.rep(sep = ",").map { _ => () }
  val ExpressionList: Parser[Unit] = Expression.rep(sep = ",")

  val suchThing = {
    (identifier ~ tpe.? ~ "=" ~ lineDelimiter.rep ~ Expression).map(VarBinding.tupled)
  }

  val varBinding = P {
    (identifier ~ tpe.? ~ "=" ~/ lineDelimiter.rep ~ Expression).map(VarBinding.tupled)
  }

  def goVars: Parser[Seq[VarBinding]] = P {
    // "var" ~ inParens(varBinding.rep(min=1).map(_.toList))
    "var" ~/ lineDelimiter.rep ~ (
      inParens(varBinding.rep(sep = lineDelimiter)) |
        varBinding.map(Seq(_))
    )
  }

  def imports: Parser[Seq[Import]] =
    importSingle | importMultiple

  def importPair: Parser[Import] = P { identifier.? ~ string_lit }.map {
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
    val contextArg: Parser[GoType] = inParens(
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
      (namedArgs | positionallyNamedArgs) ~
      funcResultArgs).map(NamedFuncDef.tupled)

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
    (CharsWhile(c => ! (charThings contains c)) | string_lit | block | comment | parenExpr).map { _ => () }
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
