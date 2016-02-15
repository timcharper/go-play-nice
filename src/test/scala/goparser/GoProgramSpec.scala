package goparser

import fastparse.core.ParseError
import fastparse.core.Parsed
import fastparse.core.Parser
import org.scalatest.{FunSpec, Matchers}
import ast._

class GoProgramSpec extends FunSpec with Matchers {
  // import GoExpr.inParens
  
  def doParse[T](p: Parser[T], s: String): T = {
    p.parse(s) match {
      case Parsed.Success(m, _) =>
        m
      case f: Parsed.Failure =>
        throw new ParseError(f)
    }
  }
  describe("program") {
    it("parses the declaration and stuff") {
      val program = doParse(GoProgram.program, """
// Copyright (C) 2016 Space Monkey, Inc.

package hancock

import (
	"io"
	"time"

	"sm/protos/proto"
	"sm/types"
)

const (
  a = 1
)

var (
	KeyMismatchError         = VerifyError.NewClass("key mismatch")
	CertificateNotFoundError = VerifyError.NewClass("certificate not found")
	IssuerNotFoundError      = VerifyError.NewClass("issuer not found")
	CyclicalTrustError       = VerifyError.NewClass("cyclical trust")
	UntrustedRootError       = VerifyError.NewClass("root is not trusted")
	CertificateExpiredError  = VerifyError.NewClass("certificate expired")
	FieldMissingError        = VerifyError.NewClass("field missing")
)

// MakeCertificateTemplate is a convenience method to create a
// Certificate ready for signing. The signature algorithm is set to the
// default for the private key.
func MakeCertificateTemplate(id types.Binary, private_key *PrivateKey) (
	template *CertificateFields) {

	return &CertificateFields{
		Id:        []byte(id),
		PublicKey: private_key.PublicKey}
}

// CreateCertificate creates a certificate signed by the issuer using the
// default signature algorithm for the private key.
func CreateCertificate(fields *CertificateFields, issuer *Certificate,
	issuer_key *PrivateKey) (*Certificate, error) {

	return CreateCertificateWith(fields, issuer, issuer_key,
		issuer_key.DefaultSignatureAlgorithm())
}

// CreateCertificate creates a certificate signed by the issuer using the
// supplied signature algorithm. If the algorithm is is incompatible with the
// issuer private key, an error will be returned.
func CreateCertificateWith(fields *CertificateFields, issuer *Certificate,
	issuer_key *PrivateKey, signature_algorithm Signature_Algorithm) (
	*Certificate, error) {

	err := checkKeyBelongsToCertificate(issuer_key, issuer)
	if err != nil {
		return nil, err
	}

	return createCertificate(fields, issuer.Fields.Id, issuer_key,
		signature_algorithm)
}

// CreateRootCertificate is a convenience methed to create a self-signed
// certificate for the provided private_key.
func CreateRootCertificate(fields *CertificateFields,
	private_key *PrivateKey) (*Certificate, error) {

	return CreateRootCertificateWith(fields, private_key,
		private_key.DefaultSignatureAlgorithm())
}

// CreateRootCertificate is a convenience methed to create a self-signed
// certificate for the provided private_key.
func CreateRootCertificateWith(fields *CertificateFields,
	private_key *PrivateKey, signature_algorithm Signature_Algorithm) (
	*Certificate, error) {

	return createCertificate(fields, fields.Id, private_key,
		signature_algorithm)
}

// createCertificate facilitates creating a signed certificate
func createCertificate(fields *CertificateFields, issuer_id []byte,
	issuer_key *PrivateKey, signature_algorithm Signature_Algorithm) (
	*Certificate, error) {

	// make sure the certificate has all of the required fields
	switch {
	case len(fields.Id) == 0:
		return nil, FieldMissingError.New("certificate id")
	case len(fields.PublicKey.Key) == 0:
		return nil, FieldMissingError.New("certificate public key")
	case len(issuer_id) == 0:
		return nil, FieldMissingError.New("issuer id")
	}

	var certificate Certificate
	proto.Merge(&certificate.Fields, fields)
	certificate.Fields.IssuerId = []byte(string(issuer_id))

	// According the the protobuf spec, marshalling is done in field number
	// order. As long as the protobuf implementation is conformant and all
	// unknown fields are discarded (as is the case in all reference
	// implementations) then the marshalled protobuf should be identical
	// across implementations.
	marshalled_fields, err := certificate.Fields.Marshal()
	if err != nil {
		return nil, err
	}

	signature, err := issuer_key.SignWith(marshalled_fields,
		signature_algorithm)
	if err != nil {
		return nil, err
	}
	certificate.Signature = *signature
	return &certificate, nil
}

var _ Signatory = (*CertificateSignatory)(nil)

// GenerateKey generates a new private key using the same public key algorithm
// as the signatory.
func (s *CertificateSignatory) GenerateKey(rand io.Reader) (
	*PrivateKey, error) {

	return s.PrivateKey.PublicKey.Algorithm.GenerateKey(rand)
}

// Sign signs a message with the signatory private key and the default
// signature algorithm for the key.
func (s *CertificateSignatory) Sign(message []byte) (*SignedMessage, error) {
	return s.SignWith(message, s.PrivateKey.DefaultSignatureAlgorithm())
}

// SignWith signs a message with the signatory private key and the specified
// signature algorithm.
func (s *CertificateSignatory) SignWith(message []byte,
	signature_algorithm Signature_Algorithm) (*SignedMessage, error) {

	return SignCertificateSignedMessageWith(message, &s.PrivateKey,
		&s.Certificate, s.Chain, signature_algorithm)
}

func (s *CertificateSignatory) Verify(message *SignedMessage) error {
	return VerifyCertificateSignedMessage(message, s.bundleCertificates())
}

func (s *CertificateSignatory) Bundle() *CertificateBundle {
	return &CertificateBundle{
		Certificates: s.bundleCertificates(),
	}
}

func (s *CertificateSignatory) bundleCertificates() []*Certificate {
	return append([]*Certificate{&s.Certificate}, s.Chain...)
}

// SignCertificateSignedMessageWith signs a message using the supplied private
// key and default signature algorithm for the private key.
func SignCertificateSignedMessage(message []byte, private_key *PrivateKey,
	certificate *Certificate, chain []*Certificate) (*SignedMessage, error) {

	return SignCertificateSignedMessageWith(message, private_key, certificate,
		chain, private_key.DefaultSignatureAlgorithm())
}

// SignCertificateSignedMessageWith signs a message using the supplied private
// key and signature algorithm.
func SignCertificateSignedMessageWith(message []byte, private_key *PrivateKey,
	certificate *Certificate, chain []*Certificate,
	signature_algorithm Signature_Algorithm) (*SignedMessage, error) {

	err := checkKeyBelongsToCertificate(private_key, certificate)
	if err != nil {
		return nil, err
	}

	signature, err := private_key.SignWith(message, signature_algorithm)
	if err != nil {
		return nil, err
	}

	return &SignedMessage{
		Message:   []byte(string(message)),
		Signature: *signature,
		CertificateProof: &CertificateProof{
			SignatoryId:  certificate.Fields.Id,
			Certificates: append([]*Certificate{certificate}, chain...),
		},
	}, nil
}

// VerifyCertificateSignedMessage verifies a signed message with a certificate
// proof. A chain of trust is constructed via certificates held in the
// certificate proof and must root back to a certificate in the trusted pool.
func VerifyCertificateSignedMessage(message *SignedMessage,
	trusted_pool []*Certificate) error {

	if message.CertificateProof == nil {
		return ProofMismatchError.New("expecting certificate proof")
	}

	signatory_certificate, err := VerifyCertificate(
		types.Binary(message.CertificateProof.SignatoryId),
		message.CertificateProof.Certificates, trusted_pool)
	if err != nil {
		return err
	}

	return message.Signature.Verify(&signatory_certificate.Fields.PublicKey,
		message.Message)
}

// VerifyCertificate verifies a certificate chain starting with the certificate
// identified by the leaf_id. A chain of trust is constructed via certificates
// passed in chain must root back to a certificate in the trusted pool.
func VerifyCertificate(leaf_id types.Binary, chain []*Certificate,
	trusted_roots []*Certificate) (leaf *Certificate, err error) {

	pool := map[types.Binary]*Certificate{}
	// Add in the chain certificates first.
	for _, certificate := range chain {
		certificate_id := types.Binary(certificate.Fields.Id)
		pool[certificate_id] = certificate
	}
	// Add in the roots (overwriting any from the chain with the same id). It
	// is important that these are added after the chain certificates so the
	// check below for a trusted root using pointer comparison works properly.
	for _, certificate := range trusted_roots {
		certificate_id := types.Binary(certificate.Fields.Id)
		pool[certificate_id] = certificate
	}

	certificate_id := leaf_id
	certificate, ok := pool[certificate_id]
	if !ok {
		// TODO(andrew): provide an optional way for the caller to fetch the
		// certificate for some other location.
		return nil, CertificateNotFoundError.New("%x", certificate_id)
	}
	leaf = certificate

	// to check for a cyclical chain
	visited := map[types.Binary]bool{}

	var issuer_id types.Binary
	var issuer *Certificate
	for {
		if visited[certificate_id] {
			return nil, CyclicalTrustError.New("%x", certificate_id)
		}
		visited[certificate_id] = true

		issuer_id = types.Binary(certificate.Fields.IssuerId)
		issuer = pool[issuer_id]
		if issuer == nil {
			// TODO(andrew): provide an optional way for the caller to fetch
			// the certificate for some other location.
			return nil, IssuerNotFoundError.New("%x", issuer_id)
		}

		err = verifyCertificate(certificate, issuer)
		if err != nil {
			return nil, err
		}

		if certificate.Fields.ExpiresAt != nil {
			if time.Now().After(time.Unix(*certificate.Fields.ExpiresAt, 0)) {
				return nil, CertificateExpiredError.New("%x expired at %s",
					certificate_id, certificate.Fields.ExpiresAt)
			}
		}

		// Is this a self-signed certificate?
		if certificate_id == issuer_id {
			// The end of the chain of trust has been reached. Check to see if
			// the root certificate is trusted.
			for _, trusted_root := range trusted_roots {
				// Pointer equality.... deliberate.
				if trusted_root == issuer {
					return leaf, nil
				}
			}
			return nil, UntrustedRootError.New("%x", issuer_id)
		}
		certificate_id = issuer_id
		certificate = issuer
	}
}

// verifyCertificate verifies the signature on a certificate using the public
// key of the issuer.
func verifyCertificate(certificate, issuer *Certificate) error {
	marshalled, err := certificate.Fields.Marshal()
	if err != nil {
		return err
	}

	return certificate.Signature.Verify(&issuer.Fields.PublicKey, marshalled)
}

// checkKeyBelongsToCertificate makes sure the private key belongs to the
// public key in the certificate.
func checkKeyBelongsToCertificate(private_key *PrivateKey,
	certificate *Certificate) error {

	if !proto.Equal(&private_key.PublicKey, &certificate.Fields.PublicKey) {
		return KeyMismatchError.New(
			"private key does not belong to certificate %x",
			certificate.Fields.Id)
	}
	return nil
}

var _ Verifier = (*CertificateBundle)(nil)

// Verify verifies a signed message using the certificates in the bundle
// as trusted certificates.
func (bundle *CertificateBundle) Verify(signed_message *SignedMessage) error {
	return VerifyCertificateSignedMessage(signed_message, bundle.Certificates)
}
""")

      program.pkgName shouldBe ("hancock")
    }
  }
}
