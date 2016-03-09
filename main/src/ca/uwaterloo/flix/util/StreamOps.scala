package ca.uwaterloo.flix.util

import java.io.{ByteArrayOutputStream, InputStream}

object StreamOps {

  /**
    * Reads an array of all bytes read from the given input stream `is`.
    */
  def readAllBytes(is: InputStream): Array[Byte] = {
    val os = new ByteArrayOutputStream()

    val buffer = new Array[Byte](0xFFFF)

    var read: Int = is.read(buffer)
    do {
      os.write(buffer, 0, read)
      read = is.read(buffer)
    } while (read != -1)

    os.toByteArray
  }

}
