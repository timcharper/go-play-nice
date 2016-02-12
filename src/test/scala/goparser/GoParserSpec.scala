package goparser

import org.scalatest.{FunSpec, Matchers, Inside}

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
      doParse(GoParser.pkg, "package thing") shouldBe Package("thing")
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

      test("*int") shouldBe (PointerType(NamedType("int")))
      test("int") shouldBe (NamedType("int"))
      test("[]int") shouldBe (SliceType(None, NamedType("int")))
      test("[10]int") shouldBe (SliceType(Some(10), NamedType("int")))
      test("[10]*int") shouldBe (SliceType(Some(10), PointerType(NamedType("int"))))
      test("map[string]int") shouldBe (MapType(NamedType("string"), NamedType("int")))
    }

    it("parses a struct field") {
      doParse(Lexical.structField, "Key       []byte    `protobuf:\"bytes,1,opt,name=key\"`\n") shouldBe (
        StructField("Key", SliceType(None, NamedType("byte")), Some("""protobuf:"bytes,1,opt,name=key"""")))

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
  }
}
