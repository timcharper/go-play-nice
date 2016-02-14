package goparser

import java.io.File
import ast._
import fastparse.noApi._

case class GoPackage(
  path: File,
  typeDefs: Map[String, TypeDef],
  funcs: Map[String, FuncType]
)


object GoProgram {
  import Lexical._
  import GoExpr._
  import GoParser._
  import WsApi._


  def program = P {
    packageDef
  }

}

