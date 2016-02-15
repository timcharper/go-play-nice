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
      doParse(GoParser.varDef, """var Error    = errors.NewClass("demo")""") shouldBe (
        Seq(VarBinding("Error",None)))
    }
  }

  it("namedArgs") {
    doParse(GoExpr.namedArgs, "(src, dst []byte)") shouldBe (
      List(
        FuncArg("src", tpe("[]byte")),
        FuncArg("dst", tpe("[]byte"))))
  }

  it("parses a struct field") {
    import fastparse.all._
    doParse(GoExpr.structField ~ "\n", "Key       []byte    `protobuf:\"bytes,1,opt,name=key\"`\n") shouldBe (
      StructField("Key", SliceType(ByteType), Some("""protobuf:"bytes,1,opt,name=key"""")))

    doParse(GoExpr.structInclude ~ "\n", "IncludeThis\n") shouldBe (
      StructInclude(tpe("IncludeThis"), None))
  }

  describe("type parser") {
    describe("parsing interfaces") {
      it("parses an member item") {

        doParse(GoExpr.interfaceItem, """
          BlockSize() int
        """.trim) shouldBe (
          Left(("BlockSize", FuncType(List.empty, List(tpe("int")))))
        )

        doParse(GoExpr.interfaceMember, """
          Encrypt(src, dst []byte)
        """.trim) shouldBe (
          Left(
            ("Encrypt",
              FuncType(
                List(
                  FuncArg("src", tpe("[]byte")),
                  FuncArg("dst", tpe("[]byte"))),
                List.empty))))
      }

    }
    it("parses an interface with includes") {
      // TODO - actually include interface pkg.IncludedInterface

      doParse(GoExpr.tpe, """
          interface {
            BlockSize() int
            Encrypt(src, dst []byte)
            Decrypt(src, dst []byte)
          }
        """.trim) shouldBe (
        InterfaceType(
          members = Map(
            "BlockSize" -> FuncType(List.empty, List(tpe("int"))),
            "Encrypt" -> FuncType(
              List(
                FuncArg("src", tpe("[]byte")),
                FuncArg("dst", tpe("[]byte"))),
              List.empty),
            "Decrypt" -> FuncType(
              List(
                FuncArg("src", tpe("[]byte")),
                FuncArg("dst", tpe("[]byte"))),
              List.empty)),
          includes = List.empty))
    }

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
      "interface{}" -> InterfaceType(Map.empty, List.empty),

      "interface{A(); B(int)}" ->
        InterfaceType(
          Map(
            "A" -> FuncType(Seq(), Seq()),
            "B" -> FuncType(Seq(FuncArg("arg0", tpe("int"))), Seq())), Seq()),

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
