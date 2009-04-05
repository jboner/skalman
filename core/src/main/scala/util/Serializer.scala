/**
 * Copyright (C) 2008 Scalable Solutions.
 */

package se.scalablesolutions.skalman.util

import java.io.{ObjectOutputStream, ByteArrayOutputStream, ObjectInputStream, ByteArrayInputStream}

object Serializer {
  def out(obj: AnyRef): Array[Byte] = {
    val bos = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bos)
    out.writeObject(obj)
    out.close    
    bos.toByteArray
  }

  def in(bytes: Array[Byte]): AnyRef = {
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val obj = in.readObject
    in.close
    obj
  }
}
