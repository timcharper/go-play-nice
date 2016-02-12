package goparser.ast


sealed trait Node

case class Program(pkg: Package, statements: Seq[Node]) extends Node
case class PackageDef(pkg: String) extends Node
case class Import(alias: Option[String], pkg: String) extends Node

sealed trait StructItem
case class StructField(name: String, tpe: GoType, tag: Option[String]) extends StructItem
case class StructFieldInclude(tpe: GoType, tag: Option[String]) extends StructItem
case class StructDef(name: String, fields: List[StructItem]) extends Node

sealed trait GoType
case class ReferencedType(pkg: Option[String], name: String) extends GoType

sealed trait GoPrimitive extends GoType
case class IntegerType(bits: Option[Int], signed: Boolean) extends GoPrimitive
case class FloatType(bits: Int) extends GoPrimitive
case class ComplexType(bits: Int) extends GoPrimitive
case object StringType extends GoPrimitive
case object BooleanType extends GoPrimitive

case class UnsupportedType(n: String) extends GoType
case class SliceType(size: Option[Int], values: GoType) extends GoType
case class MapType(keyType: GoType, valueType: GoType) extends GoType
case class PointerType(tpe: GoType) extends GoType
