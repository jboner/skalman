/**
 * Copyright (C) 2008 Scalable Solutions.
 */

package se.scalablesolutions.skalman.util

import java.util.concurrent.{Executors, ScheduledFuture, ThreadFactory, TimeUnit}
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.{List => JList}

import scala.actors._
import scala.actors.Actor._
import scala.collection.jcl.{BufferWrapper,SetWrapper}

class SystemFailure(cause: Throwable) extends RuntimeException(cause)

object Helpers {
  val DOUBLE_EQUALS_DELTA_GRANULARITY = 0.0001D

  // ================================================
  /*implicit def canToRichCan[T](can: Can[T]): RichCan[T] = new RichCan[T](can)
 
  class RichCan[T](val can: Can[T]) {

    def getOrReturnNull: T = can match {
      case Full(canned) => canned
      case Failure(_, _, _) => null.asInstanceOf
      case Empty => null.asInstanceOf
    }

    def getOrReportFailure(handler : PartialFunction[String, T]): T = can match {
      case Full(canned) => canned
      case Failure(message, Full(e), _) if e.isInstanceOf[RuntimeException] => throw new SystemFailure(e)
      case Failure(message, Empty, _) => handler(message)
      case Empty => handler("Result is Empty")
      case wrapper => handler("Unexpected wrapper: " + wrapper)
    }
  }*/

  // ================================================
  def assertEquals(actual: Double, expected: Double): Unit = assertEquals(actual, expected, DOUBLE_EQUALS_DELTA_GRANULARITY)
  def assertEquals(actual: Double, expected: Double, delta: Double): Unit = 
    if ((java.lang.Double.isInfinite(expected) && !(expected == actual)) || !(java.lang.Math.abs(expected - actual) <= delta)) {
      error("Actual Double [" + actual + "] is not equal to Expected Double [" + expected + "] within delta [" + delta + "]")
    }

  // ================================================
  /*
  implicit def javaList2RichList[T](list: JList[T]): RichList[T] = new RichList(list)
  class RichList[T](val list: JList[T]) {
    def toList[T]: List[T] = list.toArray.toList.asInstanceOf[List[T]]
  }
  */
  implicit def list2RichScalaList[D](list: List[D]): RichScalaList[D] = new RichScalaList(list)
  class RichScalaList[T](val list: List[T]) {
    def toJavaList: JList[T] = java.util.Arrays.asList[T](list.toArray:_*)
  }
  
  // ================================================
  // converts java collections to scala collections
  //   implicit def setToWrapper[A](set: java.util.Set[A]) = new SetWrapper[A]{override def underlying = set}
  //   implicit def listToWrapper[A](list: java.util.List[A]) = new BufferWrapper[A]{override def underlying = list}

  // ================================================
   class ReadWriteLock {
    private val rwl = new ReentrantReadWriteLock
    private val readLock = rwl.readLock
    private val writeLock = rwl.writeLock

    def withWriteLock[T](f: => T): T = {
      writeLock.lock
      try {f} finally {writeLock.unlock}
    }

    def withReadLock[T](f: => T): T = {
      readLock.lock
      try {f} finally {readLock.unlock}
    }

  }

  // ================================================

  /**
   * Reference that can hold either a typed value or an exception.
   *
   * Usage:
   * <pre>
   * scala> ER(1)
   * res0: ErrRef[Int] = ErrRef@a96606
   *
   * scala> res0()
    res1: Int = 1
   *
   * scala> res0() = 3
   *
   * scala> res0()
   * res3: Int = 3
   * 
   * scala> res0() = { println("Hello world"); 3}
   * Hello world
   *
   * scala> res0()
   * res5: Int = 3
   *  
   * scala> res0() = error("Lets see what happens here...")
   *
   * scala> res0()
   * java.lang.RuntimeException: Lets see what happens here...
   * 	at ErrRef.apply(RefExcept.scala:11)
   * 	at .<init>(<console>:6)
   * 	at .<clinit>(<console>)
   * 	at Re...
   * </pre>
   */
  class ErrRef[S](s: S){
    private[this] var contents: Either[Throwable, S] = Right(s)
    def update(value: => S) = contents = try { Right(value) } catch { case (e : Throwable) => Left(e) }
    def apply() = contents match {
      case Right(s) => s
      case Left(e) => throw e.fillInStackTrace
    }
  }
  object ER {
    def apply[S](s: S) = new ErrRef(s)
  }
}


