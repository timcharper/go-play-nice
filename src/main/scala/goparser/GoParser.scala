package goparser

import fastparse.noApi._
import ast._
import scala.annotation.tailrec
// import Expressions._

// object Basic {
// }


object WsApi extends fastparse.WhitespaceApi.Wrapper(Lexical.wscomment)

object GoParser {
  import WsApi._
  import Lexical._
  import GoExpr._

  def packageDef: Parser[PackageDef] =
    P("package" ~ identifier).map(PackageDef)

  def structDef: Parser[StructDef] = P {
    "type" ~ identifier ~ "struct" ~/ "{" ~ lineDelimiter.rep ~
      (structItem ~ lineDelimiter.rep).rep ~
    "}"
  }.map {
    case (name, fields) => StructDef(name, fields.toList)
  }

  val varBinding = P {
    (identifier ~ tpe.? ~ "=" ~/ lineDelimiter.rep ~ Expression).map(VarBinding.tupled)
  }

  def goVars: Parser[Seq[VarBinding]] = P {
    "var" ~/ lineDelimiter.rep ~ (
      inParens(varBinding.rep(sep = `;`) ~ `;`.rep) |
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

  def importSingle: Parser[Seq[Import]] =
    ("import" ~ importPair).map(List(_))


  def namedFuncDef: Parser[FuncDef] = P {
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
      ( (namedArgs | positionallyNamedArgs) ~
        funcResultArgs).map(FuncType.tupled)
    ).map(FuncDef.tupled)

    funcDefHeader ~ block
  }

  def interfaceInclude: Parser[Either[Nothing, ReferencedType]] =
    GoExpr.referencedTpe.map(Right(_))

  def interfaceMember: Parser[Either[(String, FuncType), Nothing]] = {
    (identifier ~ GoExpr.funcTypeArgs ~ GoExpr.funcResultArgs).map {
      case (name, args, resultArgs) =>
        Left((name, FuncType(args, resultArgs)))
    }
  }

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
