package goparser

import fastparse.noApi._
import ast._
import scala.annotation.tailrec

object GoExpr {
  import WsApi._
  import Lexical._

  val `[`: Parser[Unit] = P { "[" ~ lineDelimiter.rep }
  val `]`: Parser[Unit] = P { "]" }
  val `(`: Parser[Unit] = P { "(" ~ lineDelimiter.rep }
  val `)`: Parser[Unit] = P { ")" }
  val `,`: Parser[Unit] = P { "," ~ lineDelimiter.rep }
  val `.`: Parser[Unit] = P { "." ~ lineDelimiter.rep }
  val `;` = (";" ~ lineDelimiter.rep) | lineDelimiter.rep(1)

  val structTpe: P0 = P {
    "struct" ~ "{" ~ lineDelimiter.rep ~ (structItem ~ lineDelimiter.rep).rep ~ "}"
  }.map(_ => ())

  val LiteralValue: P0 = P {
    "{" ~ lineDelimiter.rep ~ ElementList ~ `,`.? ~ "}"
  }

  val ElementList: P0 = P {
    Element.rep(sep = `,`)
  }

  val FunctionLit: P0 = P {
    funcTpe ~ block
  }.map(_ => ())

  val Operand: P0 = P {
    Literal | OperandName | MethodExpr | "(" ~ lineDelimiter.rep ~ Expression ~ ")"
  }

  val MethodExpr: P0 = P {
    (ReceiverType ~ "." ~ lineDelimiter.rep ~ MethodName)
  }.map(_ => ())

  val ReceiverType = identifier
  val MethodName = identifier

  val Literal: P0 = P {
    BasicLit.map(_ => ()) | CompositeLit | FunctionLit
  }

  val CompositeLit: P0 = P { LiteralType ~ LiteralValue }
  val LiteralType: P0 = P {
    structTpe | arrayTpe | ("[" ~ lineDelimiter.rep ~ "..." ~ "]" ~ tpe) |
    sliceTpe | mapTpe | referencedTpe
  }.map { _ => () }

  val Element: P0 = P { ( Key ~ ":" ).? ~ Value }
  val Key: P0 = P {  FieldName | Expression | LiteralValue }
  val FieldName: P0 = P {  identifier }.map(_ => ())
  val Value: P0 = P {  Expression | LiteralValue }

  val BasicLit: Parser[String] = P {
    int_lit | float_lit | imaginary_lit | rune_lit | string_lit
  }

  val OperandName: P0 = P {
    (identifier | QualifiedIdent).map(_ => ())
  }

  val PackageName = P { identifier }
  val QualifiedIdent = PackageName ~ "." ~ identifier

  def Expression: P0 = P {
    UnaryExpr ~ (binary_op ~ Expression).rep
  }

  val UnaryExpr: P0  = P {
    PrimaryExpr | (unary_op ~ UnaryExpr)
  }

  val Conversion: P0 = P {
    tpe ~ "(" ~ Expression ~ `,`.? ~ ")"
  }.map ( _ => () )

  val PrimaryExpr: P0 = P {
	  ( Operand |
	    Conversion) ~
	  (Selector | Index | Slice | TypeAssertion | Arguments).rep
  }

  val Selector: P0 = P { (`.` ~ identifier).map { _ => () } }
  val Index: P0 = P { `[` ~ lineDelimiter.rep ~ Expression ~ "]" }
  val Slice: P0 = P {
    `[` ~ (
      ( Expression.? ~ ":" ~ Expression.? ) |
      ( Expression.? ~ ":" ~ Expression ~ ":" ~ Expression )) ~
    "]"
  }

  val TypeAssertion: P0 = P { `.` ~ "(" ~ tpe ~ ")" }.map{ _ => () }
  val Arguments: P0 = P {
    "(" ~ ( (ExpressionList | (tpe ~ ( `,` ~ ExpressionList ).?) ) ~ "...".? ~ `,`.?).? ~ ")"
  }.map { _ => () }

  val ConstDecl: P0 = P {
    "const" ~ ( ConstSpec | (
      "(" ~ lineDelimiter.rep(1) ~ (ConstSpec.rep(sep = `;`)) ~ ")"
    ))
  }.map { _ => () }

  val ConstSpec      = P { IdentifierList ~ tpe.? ~ "=" ~ ExpressionList }

  val IdentifierList: P0 = identifier.rep(sep = `,`).map { _ => () }
  val ExpressionList: P0 = Expression.rep(sep = `,`)
  def inParens[T](p: Parser[T]): Parser[T] = `(` ~ p ~ ")"
  def maybePointer(p: Parser[GoType]): Parser[GoType] = P {
    ("*" ~ p).map(PointerType) | p
  }

  def sliceTpe: Parser[SliceType] = P {
    "[" ~ "]" ~ tpe
  } map {
    case (values) =>
      SliceType(values)
  }
  def arrayTpe: Parser[ArrayType] = P {
    "[" ~ Expression ~ "]" ~ tpe
  } map {
    case (values) =>
      ArrayType(values)
  }

  def mapTpe: Parser[MapType] = P {
    ("map" ~ `[` ~ maybePointer(sliceTpe | primitiveTpe | referencedTpe) ~ "]" ~ tpe)
  } map {
    case (keyType, valueType) =>
      MapType(keyType, valueType)
  }

  def referencedTpe: Parser[ReferencedType] = P {
    ((identifier ~ ".").? ~ identifier).map(ReferencedType.tupled)
  }


  def tpe: Parser[GoType] = P {
    maybePointer(sliceTpe | arrayTpe | mapTpe | primitiveTpe | referencedTpe)
  }


  // TODO - move these below
  def structField: Parser[StructField] = P {
    identifier ~ tpe ~ string_lit.? ~ &(lineDelimiter | "}")
  }.map(StructField.tupled)

  def structItem: Parser[StructItem] =
    structField | structFieldInclude

  def primitiveTpe =
    integerTpe | floatTpe | complexTpe | boolTpe | stringTpe | funcTpe

  def structFieldInclude: Parser[StructFieldInclude] = P {
    tpe ~ string_lit.? ~ &("\n" | "}")
  }.map(StructFieldInclude.tupled)

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

  // Just consume the contents of a block
  def contents: P0 = P {
    val charThings = Set('/', '(', ')', '{', '}', '"', '`')
    (CharsWhile(c => ! (charThings contains c)) | string_lit | block | comment | parenExpr).map { _ => () }
  }

  def parenExpr: P0 = P {
    "(" ~ contents.rep ~ ")"
  }

  def block: P0 = P {
    "{" ~ contents.rep ~ "}"
  }

}
