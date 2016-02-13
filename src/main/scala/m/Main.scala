package m
import fastparse.core.Parsed
import goparser._

object Main extends App {
  val Parsed.Success(privKey, _) = GoParser.structDef.parse("""
    type PrivateKey struct {
      Key       []byte    `protobuf:"bytes,1,opt,name=key" json:"key,omitempty"`
      PublicKey PublicKey `protobuf:"bytes,2,opt,name=public_key" json:"public_key"`
    }
  """.trim)

  val Parsed.Success(certFields, _) = GoParser.structDef.parse("""
    type CertificateFields struct {
      Name      string    `protobuf:"bytes,1,opt,name=name" json:"name"`
      Id        []byte    `protobuf:"bytes,2,opt,name=id" json:"id,omitempty"`
      IssuerId  []byte    `protobuf:"bytes,3,opt,name=issuer_id" json:"issuer_id,omitempty"`
      ExpiresAt *int64    `protobuf:"varint,4,opt,name=expires_at" json:"expires_at,omitempty"`
      PublicKey PublicKey `protobuf:"bytes,5,opt,name=public_key" json:"public_key"`
    }
  """.trim)

  println(StructFormatter.format(privKey))
  println(StructFormatter.format(certFields))

}
