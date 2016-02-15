package goparser.ast


sealed trait Node
sealed trait DefNode extends Node

case class Import(alias: Option[String], pkg: String) extends DefNode

sealed trait StructItem extends Node
case class StructField(name: String, tpe: GoType, tag: Option[String]) extends StructItem
case class StructInclude(tpe: GoType, tag: Option[String]) extends StructItem

sealed trait GoType extends Node
case class StructType(fields: Seq[StructItem]) extends Node with GoType
case class InterfaceType(
  members: Map[String, FuncType],
  includes: Seq[ReferencedType]) extends Node with GoType
case class ReferencedType(pkg: Option[String], name: String) extends GoType

sealed trait GoPrimitive extends GoType
case class IntegerType(bits: Option[Int], signed: Boolean) extends GoPrimitive
case class FloatType(bits: Int) extends GoPrimitive
case class ComplexType(bits: Int) extends GoPrimitive
case object StringType extends GoPrimitive
case object ErrorType extends GoPrimitive
case object BooleanType extends GoPrimitive
case class FuncArg(
  name: String,
  tpe: GoType) extends Node

case class FuncType(args: Seq[FuncArg], results: Seq[GoType]) extends GoType

sealed trait ListType extends GoType {
  val values: GoType
}

case class SliceType(values: GoType) extends GoType with ListType
case class ArrayType(values: GoType) extends GoType with ListType
case class MapType(keyType: GoType, valueType: GoType) extends GoType
case class PointerType(tpe: GoType) extends GoType

case class PackageDef(pkg: String) extends Node
case class FuncDef(
  context: Option[GoType],
  name: String,
  func: FuncType) extends DefNode

case class TypeDef(name: String, tpe: GoType) extends DefNode

case class VarBinding(name: String, t: Option[GoType]) extends DefNode
