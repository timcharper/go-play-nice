package goparser
sealed trait Node

case class Program(pkg: Package, statements: Seq[Node]) extends Node
case class Package(pkg: String) extends Node
case class Import(alias: Option[String], pkg: String) extends Node

sealed trait StructItem
case class StructField(name: String, tpe: GoType, tag: Option[String]) extends StructItem
case class StructFieldInclude(tpe: GoType, tag: Option[String]) extends StructItem
case class StructDef(name: String, fields: List[StructItem]) extends Node

sealed trait GoType
case class SliceType(size: Option[Int], values: GoType) extends GoType
case class MapType(keyType: GoType, valueType: GoType) extends GoType
case class NamedType(name: String) extends GoType
case class PointerType(tpe: GoType) extends GoType
