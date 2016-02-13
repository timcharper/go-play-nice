package goparser

import org.scalatest.{FunSpec, Matchers, Inside}
import ast._

class GoParserSpec extends FunSpec with Matchers with Inside {

  // def testing[T](fn: parser.type => parser.Parser[T], s: String): T =
  //   parser.parseAll(fn(parser), s).get

  import fastparse.core._
  def doParse[T](p: Parser[T], s: String): T = {
    p.parse(s) match {
      case Parsed.Success(m, _) =>
        m
      case f: Parsed.Failure =>
        throw new ParseError(f)
    }
  }
  val tpe = doParse(Lexical.tpe, _: String)

  import fastparse.all.Parsed
  describe("elements") {
    it("parsers a package line") {
      doParse(GoParser.pkg, "package thing") shouldBe PackageDef("thing")
    }

    it("parses a single input line without an alias") {
      doParse(GoParser.imports, """import "thing"""") shouldBe (
        Seq(Import(None, "thing")))
    }

    it("parses a single input line with an alias") {
      doParse(GoParser.imports, """import some "thing"""") shouldBe (
        Seq(Import(Some("some"), "thing")))
    }

    it("parses a multi-line import statement") {
      doParse(GoParser.imports, """
        import (
          some "thing"
          other "thing"
          "noalias"
        )
        """.trim) shouldBe (
        Seq(
          Import(Some("some"), "thing"),
          Import(Some("other"), "thing"),
          Import(None, "noalias")))
    }

    it("parses types") {
      val test = doParse(Lexical.tpe, _: String)

      // ints
      test("byte") shouldBe (IntegerType(Some(8), false))
      test("int") shouldBe (IntegerType(None, true))
      test("uint32") shouldBe (IntegerType(Some(32), false))
      test("uint64") shouldBe (IntegerType(Some(64), false))
      test("float32") shouldBe (FloatType(32))
      test("float64") shouldBe (FloatType(64))
      test("complex64") shouldBe (ComplexType(64))
      test("complex128") shouldBe (ComplexType(128))
      test("boolean") shouldBe (BooleanType)
      // pointers
      test("*int") shouldBe (PointerType(IntegerType(None, true)))
      // slices
      test("[]int") shouldBe (SliceType(None, IntegerType(None, true)))
      test("[10]int") shouldBe (SliceType(Some(10), IntegerType(None, true)))
      test("[10]*int") shouldBe (SliceType(Some(10), PointerType(IntegerType(None, true))))
      test("map[string]int") shouldBe (MapType(StringType, IntegerType(None, true)))
      test("Message") shouldBe (ReferencedType(None, "Message"))
      test("protobuf.Message") shouldBe (ReferencedType(Some("protobuf"), "Message"))
    }

    it("parses a struct field") {
      doParse(Lexical.structField, "Key       []byte    `protobuf:\"bytes,1,opt,name=key\"`\n") shouldBe (
        StructField("Key", SliceType(None, ByteType), Some("""protobuf:"bytes,1,opt,name=key"""")))

      doParse(Lexical.structFieldInclude, "IncludeThis\n") shouldBe (
        StructFieldInclude(tpe("IncludeThis"), None))
    }

    it("parse a struct") {
      doParse(GoParser.struct, """
        type PrivateKey struct {
          Key       []byte    `protobuf:"bytes,1,opt,name=key" json:"key,omitempty"`
          PublicKey PublicKey `protobuf:"bytes,2,opt,name=public_key" json:"public_key"`
          IncludeThis
        }
      """.trim) shouldBe (
        StructDef(
          "PrivateKey",
          List(
            StructField("Key", tpe("[]byte"), Some("""protobuf:"bytes,1,opt,name=key" json:"key,omitempty"""")),
            StructField("PublicKey", tpe("PublicKey"), Some("""protobuf:"bytes,2,opt,name=public_key" json:"public_key"""")),
            StructFieldInclude(tpe("IncludeThis"), None))))
    }

    it("consumes a block of code") {
      doParse(GoParser.block, """
        {
          in := reflect.ValueOf(src)
          out := reflect.ValueOf(dst)
          if out.IsNil() {
            panic("proto: nil destination")
          }
          if in.Type() != out.Type() {
            // Explicit test prior to mergeStruct so that mistyped nils will fail
            panic("proto: type mismatch")
          }
          if in.IsNil() {
            // Merging nil into non-nil is a quiet no-op
            return
          }
          mergeStruct(out.Elem(), in.Elem())
        }
      """.trim) shouldBe (())
    }

    it("parses a function with a body and without return values") {
      doParse(GoParser.namedFunctionDef, """
        func Merge(dst, src Message) {}
      """.trim) shouldBe (
        NamedFunctionDef(
          None,
          "Merge",
          List(
            FunctionArg("dst", ReferencedType(None, "Message")),
            FunctionArg("src", ReferencedType(None, "Message"))),
          List.empty)
      )
    }
  }
}
