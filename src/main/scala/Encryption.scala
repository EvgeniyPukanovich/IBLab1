import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

object Encryption {
  val key = "enIntVecTest2020"
  val initVector = "encryptionIntVec"

  def encrypt(bytes: Array[Byte]) = {
    val key = "bad8deadcafef00d"
    val skeySpec = new SecretKeySpec(key.getBytes, "AES")
    val  cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec)
    cipher.doFinal(bytes)
  }

  def decrypt(text:String) :String={
    val  iv = new IvParameterSpec(initVector.getBytes("UTF-8"))
    val  skeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES")

    val  cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
    cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv)
    val  original = cipher.doFinal(Base64.getDecoder.decode(text))

    new String(original)
  }
}