package goparser

import java.io.File
import ast._
import fastparse.noApi._

case class GoFile(
  pkgName: String,
  imports: Map[String, String],
  typeDefs: Map[String, GoType],
  funcs: Map[String, FuncType],
  contextFuncs: Map[GoType, Map[String, FuncType]]
)

object GoProgram {
  import GoParser._
  import WsApi._
  import Lexical._
  // import Lexical.wscomment

  val defNodes: Parser[Seq[DefNode]] = {
    imports | typeDef | funcDef.rep(1) | varDef | constDef
  }

  import GoExpr.`;`
  def program: Parser[GoFile] = P {
    ((lineComment | lineDelimiter).rep ~ packageDef ~ `;` ~ defNodes.rep(sep = `;`).map(_.flatten)) ~ `;`.rep ~ End
  }.map { case (PackageDef(nme), defs) =>
      val contextFuncs = defs.
        collect { case FuncDef(Some(ctx), name, func) => (ctx, name, func) }.
        groupBy(_._1).mapValues { vs =>
          vs.map { case (_, name, func) => (name, func) }.toMap
        }

      GoFile(
        nme,
        imports = defs.collect { case Import(alias, pkg) => (alias.getOrElse(pkg.split("/").last), pkg) }.toMap,
        typeDefs = defs.collect { case TypeDef(name, tpe) => (name, tpe) }.toMap,
        funcs = defs.collect { case FuncDef(None, name, func) => (name, func) }.toMap,
        contextFuncs = contextFuncs
      )
  }
}

