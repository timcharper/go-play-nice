package goparser

import org.scalatest.{FunSpec, Matchers, Inside}
import ast._

class GoParserSpec extends FunSpec with Matchers with Inside with ParseHelpers{

  // def testing[T](fn: parser.type => parser.Parser[T], s: String): T =
  //   parser.parseAll(fn(parser), s).get

  describe("const") {
    it("parses a const block") {
      doParse(GoParser.constDef, """
        const (
          PublicKey_INVALID PublicKey_Algorithm = 0
          PublicKey_ED25519 PublicKey_Algorithm = 1
        )
      """.trim)
      // presently nothing returned..
    }
  }

  describe("var") {
    it("parses a single variable without a type") {
      doParse(GoParser.varDef, """var LosError = thing.Error""") shouldBe (
        Seq(VarBinding("LosError", None)))
    }

    it("parses a single variable with a type") {
      doParse(GoParser.varDef, """var LosNumero int64 = 24""") shouldBe (
        Seq(VarBinding("LosNumero", Some(tpe("int64")))))
    }

    it("parses multiple global variables") {
      doParse(GoParser.varDef, """
        var (
          Error    = errors.NewClass("demo")
          NotFound = Error.NewClass("not found",
            errhttp.SetStatusCode(httplib.StatusNotFound))
          Unauthorized = Error.NewClass("unauthorized",
            errhttp.SetStatusCode(httplib.StatusUnauthorized))
        )
      """.trim) shouldBe (
        Seq(
          VarBinding("Error", None),
          VarBinding("NotFound", None),
          VarBinding("Unauthorized", None)
        ))
    }
  }

  describe("type def parsing") {
    it("parses a type alias") {
      doParse(GoParser.typeDef, """
        type PrivateKey string
      """.trim).head shouldBe (
        TypeDef(
          "PrivateKey",
          tpe("string")))
    }

    it("parses a struct definition") {
      doParse(GoParser.typeDef, """
        type PrivateKey struct {
          Key       []byte    `protobuf:"bytes,1,opt,name=key" json:"key,omitempty"`
          PublicKey PublicKey `protobuf:"bytes,2,opt,name=public_key" json:"public_key"`
          IncludeThis
        }
      """.trim).head shouldBe (
        TypeDef(
          "PrivateKey",
          StructType(
            Seq(
              
              StructField("Key", tpe("[]byte"), Some("""protobuf:"bytes,1,opt,name=key" json:"key,omitempty"""")),
              StructField("PublicKey", tpe("PublicKey"), Some("""protobuf:"bytes,2,opt,name=public_key" json:"public_key"""")),
              StructInclude(tpe("IncludeThis"), None)))))
    }

  }

  describe("elements") {
    it("parses a package line") {
      doParse(GoParser.packageDef, "package thing") shouldBe PackageDef("thing")
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

    it("consumes a block of code") {
      doParse(GoExpr.block, """
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

    it("parses a func with a body and without return values") {
      doParse(GoParser.funcDef, """
        func Merge(dst, src Message) {}
      """.trim) shouldBe (
        FuncDef(
          None,
          "Merge",
          FuncType(
            List(
              FuncArg("dst", ReferencedType(None, "Message")),
              FuncArg("src", ReferencedType(None, "Message"))),
            List.empty))
      )
    }

    it("parses a more complex func def") {
      val f = doParse(GoParser.funcDef, """
        func MakeCertificateTemplate(id, private_key *PrivateKey) (CertificateFields) {}
      """.trim)

      f.name shouldBe("MakeCertificateTemplate")
    }


  }
}
