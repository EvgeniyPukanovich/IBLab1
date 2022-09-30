import java.io.File

import scala.collection.mutable.ArrayBuffer
import java.nio.file.{Files, Paths}
import java.io.PrintWriter


object Main extends App {
  val n = 16

  if (args.length < 2)
    println("Not enough arguments")
  else {
    val arg1 = args(0)
    val arg2 = args(1)
    executeCommand(arg1, arg2)
  }

  def executeCommand(arg1: String, arg2: String) = {
    arg1 match {
      case "prepare" =>
        val bytes = Files.readAllBytes(Paths.get(arg2))
        val extended = extend(bytes)
        val dict = createDict
        Files.write(Paths.get(arg2 + "ext"), extended.toArray)
        Files.write(Paths.get(arg2 + "dict"), dict.toArray)
      case "encode" =>
        val fileExtBytes = Files.readAllBytes(Paths.get(arg2 + "ext"))
        val dictBytes = Files.readAllBytes(Paths.get(arg2 + "dict"))
        Files.write(Paths.get(arg2 + "ext"), Encryption.encrypt(fileExtBytes))
        Files.write(Paths.get(arg2 + "dict"), Encryption.encrypt(dictBytes))
      case "translate" =>
        val dictEnc = Files.readAllBytes(Paths.get(arg2 + "dict"))
        val dict = createDict
        val file = new File(arg2)
        val par = new File(file.getAbsolutePath).getParent
        printMap(par, translate(dict.toSeq, dictEnc))
      case "decode" =>
        val fileExtBytes = Files.readAllBytes(Paths.get(arg2 + "ext"))
        val dictBytes = Files.readAllBytes(Paths.get(arg2 + "dict"))
        val dict = createDict
        val tr = translate(dict.toSeq, dictBytes)
        val decoded = decode(fileExtBytes, tr)
        val norm = extendedToNormal(decoded.toSeq)
        Files.write(Paths.get(arg2), norm.toArray)
      case _ => println("wrong arguments")
    }
  }

  def byteToSpecArr(byte: Byte) = {
    val tail: List[Byte] = List.fill(n - 1)(0)
    byte :: tail
  }

  def extend(array: Seq[Byte]) = {
    array.flatMap(b => byteToSpecArr(b))
  }

  def createDict = {
    val buffer = ArrayBuffer[Byte]()
    for (i <- 0 to 255)
      buffer ++= byteToSpecArr(i.toByte)
    buffer
  }

  def translate(originalArray: Seq[Byte], encodedArray: Seq[Byte]) = {
    val groupedOrig = originalArray.grouped(16).toList
    val groupedEnc = encodedArray.grouped(16).toList
    val zipped = groupedEnc.zip(groupedOrig).toMap
    zipped
  }

  def printMap(path: String, map: Map[Seq[Byte], Seq[Byte]]) = {
    val writer = new PrintWriter(path, "UTF-8")
    for {(key, value) <- map} {
      writer.println(key.toString() + " -> " + value.toString())
    }
    writer.close()
  }

  def decode(encodedExt: Seq[Byte], decodeMap: Map[Seq[Byte], Seq[Byte]]) = {
    val grouped = encodedExt.grouped(16).toList
    val buffArr = ArrayBuffer[Byte]()

    for (group <- grouped) {
      buffArr ++= decodeMap(group)
    }
    buffArr
  }

  def extendedToNormal(extended: Seq[Byte]) = {
    extended.grouped(16).toList.map(_.head)
  }

}