package goparser

import fastparse.noApi._
import ast._
import scala.annotation.tailrec

object GoTypes {
  import WsApi._
  import Lexical._

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
    "[" ~ GoParser.Expression ~ "]" ~ tpe
  } map {
    case (values) =>
      ArrayType(values)
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
    maybePointer(sliceTpe | arrayTpe | mapTpe | primitiveTpe | referencedTpe)
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

  val `[`: Parser[Unit] = P { "[" ~ lineDelimiter.rep }
  val `(`: Parser[Unit] = P { "(" ~ lineDelimiter.rep }
  val `,`: Parser[Unit] = P { "," ~ lineDelimiter.rep }
  val `.`: Parser[Unit] = P { "." ~ lineDelimiter.rep }
  val `;` = (";" ~ lineDelimiter.rep) | lineDelimiter.rep(1)
  def inParens[T](p: Parser[T]): Parser[T] = `(` ~ p ~ ")"

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
