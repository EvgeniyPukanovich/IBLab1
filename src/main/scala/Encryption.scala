import javax.crypto.{Cipher, KeyGenerator, SecretKey}
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

object Encryption {
  val key = generateKey

  def encrypt(bytes: Array[Byte]): Array[Byte] = {
    val skeySpec = new SecretKeySpec(key.getEncoded, "AES")
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec)
    cipher.doFinal(bytes)
  }

  def generateKey: SecretKey = {
    val keyGenerator = KeyGenerator.getInstance("AES")
    keyGenerator.init(128)
    keyGenerator.generateKey
  }
}