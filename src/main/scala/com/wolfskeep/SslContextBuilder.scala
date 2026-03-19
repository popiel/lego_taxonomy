package com.wolfskeep

import java.security.{KeyPair, KeyPairGenerator, KeyStore, PrivateKey, Security}
import java.security.cert.X509Certificate
import java.util.Date

import javax.net.ssl.{SSLContext, TrustManagerFactory, KeyManagerFactory}
import org.bouncycastle.x509.X509V3CertificateGenerator

object SslContextBuilder {

  def buildSslContext(): SSLContext = {
    val keyPair = generateKeyPair()
    val certificate = generateCertificate(keyPair)
    val keyStore = createKeyStore(keyPair.getPrivate, certificate)

    val keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    keyManagerFactory.init(keyStore, "password".toCharArray)

    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    trustManagerFactory.init(keyStore)

    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(keyManagerFactory.getKeyManagers, trustManagerFactory.getTrustManagers, null)
    sslContext
  }

  private def generateKeyPair(): KeyPair = {
    val keyPairGenerator = KeyPairGenerator.getInstance("RSA")
    keyPairGenerator.initialize(2048)
    keyPairGenerator.generateKeyPair()
  }

  private def generateCertificate(keyPair: KeyPair): X509Certificate = {
    val startDate = new Date()
    val calendar = java.util.Calendar.getInstance()
    calendar.setTime(startDate)
    calendar.add(java.util.Calendar.YEAR, 1)
    val endDate = calendar.getTime

    val certGenerator = new X509V3CertificateGenerator()
    certGenerator.setSerialNumber(java.math.BigInteger.valueOf(1))
    certGenerator.setSubjectDN(new javax.security.auth.x500.X500Principal("CN=localhost"))
    certGenerator.setIssuerDN(new javax.security.auth.x500.X500Principal("CN=localhost"))
    certGenerator.setNotBefore(startDate)
    certGenerator.setNotAfter(endDate)
    certGenerator.setPublicKey(keyPair.getPublic)
    certGenerator.setSignatureAlgorithm("SHA256withRSA")

    certGenerator.generate(keyPair.getPrivate)
  }

  private def createKeyStore(privateKey: PrivateKey, certificate: X509Certificate): KeyStore = {
    val keyStore = KeyStore.getInstance(KeyStore.getDefaultType)
    keyStore.load(null, null)

    val keyPassword = "password".toCharArray

    keyStore.setKeyEntry(
      "lego-taxonomy",
      privateKey,
      keyPassword,
      Array(certificate)
    )

    keyStore
  }
}
