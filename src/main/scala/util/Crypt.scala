/**
 * Copyright (C) 2008 Scalable Solutions.
 */

package se.scalablesolutions.skalman.util

import java.io.UnsupportedEncodingException
import java.security.{MessageDigest, NoSuchAlgorithmException}
 
object Crypt {
  val hex = "0123456789ABCDEF"
  val lineSeparator = System.getProperty("line.separator")

  def getSHA1(text: String): String = {
    val unified = unifyLineSeparator(text)
    val md = MessageDigest.getInstance("SHA-1")
    md.update(unified.getBytes("iso-8859-1"), 0, unified.length)
    val hash = md.digest
    convertToHex(hash)
  }

  def getMD5(text: String): String = {
    val unified = unifyLineSeparator(text)
    val md = MessageDigest.getInstance("MD5")
    md.update(unified.getBytes("iso-8859-1"), 0, unified.length)
    val hash = md.digest
    convertToHex(hash)
  }

  def convertToHex(bytes: Array[Byte]): String = {
    val builder = new StringBuilder
    bytes.foreach { byte => builder.append(hex.charAt((byte & 0xF) >> 4)).append(hex.charAt(byte & 0xF)) }
    builder.toString
  }

  private def unifyLineSeparator(text: String): String = text.replaceAll(lineSeparator, "\n")
}
