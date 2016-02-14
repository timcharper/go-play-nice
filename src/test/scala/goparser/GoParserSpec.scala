package goparser

import org.scalatest.{FunSpec, Matchers, Inside}
import ast._

class GoParserSpec extends FunSpec with Matchers with Inside {

  import GoTypes.inParens
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
  val tpe = doParse(GoTypes.tpe, _: String)

  import fastparse.all.Parsed
  describe("var") {
    it("parses a single variable without a type") {
      doParse(GoParser.goVars, """var LosError = thing.Error""") shouldBe (
        Seq(VarBinding("LosError", None)))
    }

    it("parses a single variable with a type") {
      doParse(GoParser.goVars, """var LosNumero int64 = 24""") shouldBe (
        Seq(VarBinding("LosNumero", Some(tpe("int64")))))
    }

    it("parses multiple global variables") {
      doParse(GoParser.goVars, """
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

  describe("Expressions") {
    it("parses a function call to a function in a package") {
      doParse(GoParser.Expression, """errors.NewClass("demo")""") shouldBe (())
    }

    it("parses a function call that spans lines") {
      doParse(inParens(GoParser.Expression), """
        (Error.NewClass("not found",
          errhttp.SetStatusCode(httplib.StatusNotFound)))
      """.trim) shouldBe (())
    }



    it("parses a var binding") {
      doParse(GoParser.varBinding, """Error    = errors.NewClass("demo")""") shouldBe (
        VarBinding("Error",None))
    }

  }

  describe("elements") {
    it("parses a package line") {
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
      doParse(GoParser.importMultiple, """
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
      // ints
      tpe("byte") shouldBe (IntegerType(Some(8), false))
      tpe("int") shouldBe (IntegerType(None, true))
      tpe("uint32") shouldBe (IntegerType(Some(32), false))
      tpe("uint64") shouldBe (IntegerType(Some(64), false))
      tpe("float32") shouldBe (FloatType(32))
      tpe("float64") shouldBe (FloatType(64))
      tpe("complex64") shouldBe (ComplexType(64))
      tpe("complex128") shouldBe (ComplexType(128))
      tpe("boolean") shouldBe (BooleanType)
      // pointers
      tpe("*int") shouldBe (PointerType(IntegerType(None, true)))
      // slices
      tpe("[]int") shouldBe (SliceType(IntegerType(None, true)))
      tpe("[10]int") shouldBe (ArrayType(IntegerType(None, true)))
      tpe("[10]*int") shouldBe (ArrayType(PointerType(IntegerType(None, true))))
      tpe("map[string]int") shouldBe (MapType(StringType, IntegerType(None, true)))
      tpe("Message") shouldBe (ReferencedType(None, "Message"))
      tpe("protobuf.Message") shouldBe (ReferencedType(Some("protobuf"), "Message"))
      tpe("func()") shouldBe (FuncType(List.empty, List.empty))
      tpe("func(int)") shouldBe (
        FuncType(
          List(
            FuncArg("arg0", tpe("int"))),
          List.empty))

      tpe("func(int) bool") shouldBe (
        FuncType(
          List(
            FuncArg("arg0", tpe("int"))),
          List(
            tpe("bool"))))

      tpe("func(src, dst []byte)") shouldBe (
        FuncType(
          List(
            FuncArg("src", tpe("[]byte")),
            FuncArg("dst", tpe("[]byte"))),
          List.empty))
    }

    it("namedArgs") {
      doParse(GoTypes.namedArgs, "(src, dst []byte)") shouldBe (
        List(
            FuncArg("src", tpe("[]byte")),
            FuncArg("dst", tpe("[]byte"))))
    }

    it("parses a struct field") {
      doParse(GoTypes.structField, "Key       []byte    `protobuf:\"bytes,1,opt,name=key\"`\n") shouldBe (
        StructField("Key", SliceType(ByteType), Some("""protobuf:"bytes,1,opt,name=key"""")))

      doParse(GoTypes.structFieldInclude, "IncludeThis\n") shouldBe (
        StructFieldInclude(tpe("IncludeThis"), None))
    }

    it("parse a struct") {
      doParse(GoParser.structDef, """
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

    it("parses a func with a body and without return values") {
      doParse(GoParser.namedFuncDef, """
        func Merge(dst, src Message) {}
      """.trim) shouldBe (
        NamedFuncDef(
          None,
          "Merge",
          List(
            FuncArg("dst", ReferencedType(None, "Message")),
            FuncArg("src", ReferencedType(None, "Message"))),
          List.empty)
      )
    }

    describe("parsing interfaces") {
      it("parses an member item") {

        doParse(GoParser.interfaceItem, """
          BlockSize() int
        """.trim) shouldBe (
          Left(("BlockSize", FuncType(List.empty, List(tpe("int")))))
        )

        doParse(GoParser.interfaceMember, """
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

      it("parses an empty definition") {
        doParse(GoParser.interfaceDef, """
          type Block interface {}
        """.trim) shouldBe (
          InterfaceDef("Block",
            members = Map.empty,
            includes = List.empty))
      }

      it("parses an interface with includes") {
        doParse(GoParser.interfaceDef, """
          type Block interface {
            BlockSize() int
            Encrypt(src, dst []byte)
            Decrypt(src, dst []byte)
          }
        """.trim) shouldBe (
          InterfaceDef("Block",
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
    }
  }
}
