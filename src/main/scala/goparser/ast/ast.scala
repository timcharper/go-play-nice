package goparser.ast


sealed trait Node
sealed trait TypeDef

case class Program(pkg: Package, statements: Seq[Node]) extends Node
case class PackageDef(pkg: String) extends Node
case class Import(alias: Option[String], pkg: String) extends Node

sealed trait StructItem extends Node
case class StructField(name: String, tpe: GoType, tag: Option[String]) extends StructItem
case class StructFieldInclude(tpe: GoType, tag: Option[String]) extends StructItem
case class StructDef(name: String, fields: List[StructItem]) extends Node with TypeDef

case class InterfaceDef(name: String,
  members: Map[String, FuncType],
  includes: List[ReferencedType]) extends Node with TypeDef

sealed trait GoType extends Node
case class ReferencedType(pkg: Option[String], name: String) extends GoType
// TODO
case class InterfaceType(members: Seq[Nothing] = List.empty) extends GoType

sealed trait GoPrimitive extends GoType
case class IntegerType(bits: Option[Int], signed: Boolean) extends GoPrimitive
case class FloatType(bits: Int) extends GoPrimitive
case class ComplexType(bits: Int) extends GoPrimitive
case object StringType extends GoPrimitive
case object BooleanType extends GoPrimitive
case class FuncArg(
  name: String,
  tpe: GoType) extends Node

case class FuncType(args: Seq[FuncArg], results: Seq[GoType]) extends GoPrimitive

case class UnsupportedType(n: String) extends GoType
case class SliceType(values: GoType) extends GoType
case class ArrayType(values: GoType) extends GoType
case class MapType(keyType: GoType, valueType: GoType) extends GoType
case class PointerType(tpe: GoType) extends GoType

case class FuncDef(
  context: Option[GoType],
  name: String,
  func: FuncType) extends Node

case class VarBinding(name: String, t: Option[GoType]) extends Node
