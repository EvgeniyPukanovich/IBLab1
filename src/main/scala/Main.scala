import scala.collection.mutable.ArrayBuffer
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object Main extends App {
  println("Hello, World!")

  val n = 16
  //Files.write(Paths.get("file.txt"), "file contents".getBytes(StandardCharsets.UTF_8))

  val origFileBytes = Files.readAllBytes(Paths.get("file.txt"))

  val r = prepare(origFileBytes)
  Files.write(Paths.get("fileExt"), r._1.toArray)
  Files.write(Paths.get("fileDict"), r._2.toArray)

  val norm = extendedToNormal(r._1)

  val fileExtBytes = Files.readAllBytes(Paths.get("fileExt"))
  val dictBytes = Files.readAllBytes(Paths.get("fileDict"))

  //Files.write(Paths.get("fileEnc.txt"), Encryption.encrypt(origFileBytes))
  val fileEnc = Encryption.encrypt(fileExtBytes)
  Files.write(Paths.get("fileExtEnc"),Encryption.encrypt(fileExtBytes) )
  val dictEnc = Encryption.encrypt(dictBytes)
  Files.write(Paths.get("fileDictEnc"), Encryption.encrypt(dictBytes))
  val tr = translate(dictBytes, dictEnc)

  val decoded = decode(fileEnc,tr)
  val or = extendedToNormal(decoded)
  val a = ""

  def prepare(array: Seq[Byte]) = {
    def byteToSpecArr(byte: Byte) = {
      val tail: List[Byte] = List.fill(n - 1)(0)
      byte :: tail
    }

    val prep = array.flatMap(b => byteToSpecArr(b))

    val buffer = ArrayBuffer[Byte]()
    for (i <- 0 to 255)
      buffer ++= byteToSpecArr(i.toByte)

    (prep, buffer)
  }

  def translate(originalArray: Seq[Byte], encodedArray: Seq[Byte]) = {
    val groupedOrig = originalArray.grouped(16).toList
    val groupedEnc = encodedArray.grouped(16).toList
    val zipped = groupedEnc.zip(groupedOrig).toMap
    zipped
  }

  def decode(encodedExt: Seq[Byte], decodeMap: Map[Seq[Byte], Seq[Byte]]) = {
    val grouped = encodedExt.grouped(16).toList
    val buffArr = ArrayBuffer[Byte]()

    for(group <- grouped){
      buffArr ++= decodeMap(group)
    }
    buffArr
  }

  def extendedToNormal(extended: Seq[Byte]) = {
    extended.grouped(16).toList.map(_.head)
  }

}