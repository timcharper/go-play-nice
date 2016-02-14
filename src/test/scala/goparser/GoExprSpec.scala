package goparser

import org.scalatest.{FunSpec, Matchers}
import ast._

class GoExprSpec extends FunSpec with Matchers with ParseHelpers {
  import GoExpr.inParens

  describe("Expressions") {
    it("parses a function call to a function in a package") {
      doParse(GoExpr.Expression, """errors.NewClass("demo")""") shouldBe (())
    }

    it("parses a function call that spans lines") {
      doParse(inParens(GoExpr.Expression), """
        (Error.NewClass("not found",
          errhttp.SetStatusCode(httplib.StatusNotFound)))
      """.trim) shouldBe (())
    }

    it("parses a var binding") {
      doParse(GoParser.varBinding, """Error    = errors.NewClass("demo")""") shouldBe (
        VarBinding("Error",None))
    }
  }

  describe("type parser") {
    // ints
    for ((tpeStr, parsedNode: GoType) <- List(
      "byte" -> IntegerType(Some(8), false),
      "int" -> IntegerType(None, true),
      "uint32" -> IntegerType(Some(32), false),
      "uint64" -> IntegerType(Some(64), false),
      "float32" -> FloatType(32),
      "float64" -> FloatType(64),
      "complex64" -> ComplexType(64),
      "complex128" -> ComplexType(128),
      "boolean" -> BooleanType,
      // pointers
      "*int" -> PointerType(IntegerType(None, true)),
      // slices
      "[]int" -> SliceType(IntegerType(None, true)),
      "[10]int" -> ArrayType(IntegerType(None, true)),
      "[10]*int" -> ArrayType(PointerType(IntegerType(None, true))),
      "map[string]int" -> MapType(StringType, IntegerType(None, true)),
      "Message" -> ReferencedType(None, "Message"),
      "protobuf.Message" -> ReferencedType(Some("protobuf"), "Message"),
      "func()" -> FuncType(List.empty, List.empty),
      "func(int)" ->
        FuncType(
          List(
            FuncArg("arg0", tpe("int"))),
          List.empty),

      "func(int) bool" -> 
        FuncType(
          List(
            FuncArg("arg0", tpe("int"))),
          List(
            tpe("bool"))),

      "func(src, dst []byte)" ->
        FuncType(
          List(
            FuncArg("src", tpe("[]byte")),
            FuncArg("dst", tpe("[]byte"))),
          List.empty))
    ) {
      it(s"parses ${tpeStr}") {
        tpe(tpeStr) shouldBe (parsedNode)
      }
    }

  }
}
