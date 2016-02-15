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
    P("package" ~/ identifier).map(PackageDef)


  def oneOrMore[T](p: Parser[T]): Parser[Seq[T]] = {
    lineDelimiter.rep ~ (
      inParens(p.rep(sep = `;`) ~ `;`.rep) |
        p.map(Seq(_))
    )
  }

  val typeDef: Parser[Seq[TypeDef]] = P {
    "type" ~/ oneOrMore {
      (identifier ~ tpe).map(TypeDef.tupled)
    }
  }

  val constDef: Parser[Seq[Nothing]] = P {
    "const" ~/ oneOrMore {
      (identifier ~ tpe.? ~ "=" ~/ lineDelimiter.rep ~ Expression).map(VarBinding.tupled)
    }
  }.map(_ => Nil)

  val varDef: Parser[Seq[VarBinding]] = P {
    "var" ~/ oneOrMore {
      (identifier ~ tpe.? ~ "=" ~/ lineDelimiter.rep ~ Expression).map(VarBinding.tupled)
    }
  }

  val imports: Parser[Seq[Import]] = P {
    "import" ~/ oneOrMore {
      (identifier.? ~ string_lit).map(Import.tupled)
    }
  }


  val funcDef: Parser[FuncDef] = P {
    val contextArg: Parser[GoType] = inParens(
      identifier.? ~ tpe
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

    val funcDefHeader = ("func" ~/ contextArg.? ~ identifier ~/
      ( (namedArgs | positionallyNamedArgs) ~
        funcResultArgs).map(FuncType.tupled)
    ).map(FuncDef.tupled)

    funcDefHeader ~ block
  }

}
