package se.scalablesolutions.skalman.util

/*
* Copyright 2007-2008 WorldWide Conferencing, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

/**
* The Can object provide methods to create Cans from:<li>
* <ul/> an Option
* <ul/> a List
* <ul/> any AnyRef object
* </li>
* It also holds implicit methods to transform: Option to Can, Can to Iterable, Can to Option
*/
object Can {
  /**
  * @returns a Can object from an Option. Full(x) if the Option is Some(x) and Empty otherwise
  */
  def apply[T](in: Option[T]) = in match {
    case Some(x) => Full(x)
    case _ => Empty
  }

  /**
  * This method is used to transform a List with 0 or one element to a Can.
  * @returns a Can object from the head of a List. Full(x) if the List contains at least one element and Empty otherwise.
  */
  def apply[T](in: List[T]) = in match {
    case x :: _ => Full(x)
    case _ => Empty
  }

  def apply[InType, OutType](pf: PartialFunction[InType, OutType])(value: InType): Can[OutType] =
  if (pf.isDefinedAt(value)) Full(pf(value)) else Empty

  def apply[InType, OutType](value: InType)(pf: PartialFunction[InType, OutType]): Can[OutType] =
  if (pf.isDefinedAt(value)) Full(pf(value)) else Empty


  /**
  * This method is used to transform any AnyRef to a Can.
  * @returns a Can object from an object. Full(in) if the object is not null and Empty otherwise.
  */
  //def apply[T <: AnyRef](in: T): Can[T] = type2Can(in)

  /**
  * This implicit def allows to use Iterable methods on a Can: size, foreach,...
  * @returns List(in) if the can is Full(in) and Nil otherwise
  */
  implicit def can2Iterable[T](in: Can[T]): Iterable[T] = in.toList

  /**
  * This implicit def allows to use Options as Cans
  * @returns a Can object from an Option. Full(in) if the Option is Some(in) and Empty otherwise
  */
  implicit def option2Can[T](in: Option[T]): Can[T] = Can(in)

  /**
  * This implicit def allows to use Cans as Options
  * @returns Some(in) if the can is Full(in) and None otherwise
  */
  implicit def can2Option[T](in: Can[T]): Option[T] = in.toOption

  /**
  * This def allows to use any object as a Can, permitting null values to be handled as Empty
  * @returns Full(in) if in is not null Empty otherwise
  */
  def legacyNullTest[T <: AnyRef](in: T): Can[T] = if (in eq null) Empty else Full(in)

  def isA[A, B](in: A, clz: Class[B]): Can[B] = in match {
    case null => Empty
    case v => Full(v).isA(clz)
  }
}

/**
* The Can class is a container which is able to declare if it is Full (with a non-null value) or Empty.
* It serves the same purpose as the Option class from Scala standard library but adds several features:<li>
* <ul> you can transform it to a Failure object if it is Empty (with the ?~ method)
* <ul> you can chain failure messages on Failure Cans
* <ul> you "run" a function on your Can, with a default value: <code>Full(1).run("zero") { x: String, y: Int => y.toString }</code>
* <ul> you can "pass" a Can to a funtion for side effects: <code>Full(1) $ { x: Can[Int] => println(x openOr 0) }</code>
* </li>
*/
@serializable
sealed abstract class Can[+A] extends Product {
  /**
  * @returns true if the Can contains no value
  */
  def isEmpty: Boolean

  /**
  * @returns false if the Can contains a value
  */
  def isDefined: Boolean = !isEmpty

  /**
  * @returns the value of the Can if it is full. Throw an exception otherwise
  */
  def open_! : A

  /**
  * @returns the value of the Can if it is full. Returns a default value otherwise
  */
  def openOr[B >: A](default: => B): B = default

  /**
  * applies a function on the Can's value if it exists
  * @returns the modified can or an Empty can
  */
  def map[B](f: A => B): Can[B] = Empty

  /**
  * applies a function returning a Can on the Can's value if it exists and removes the "inner" can if necessary
  * @returns the modified can or an Empty can
  */
  def flatMap[B](f: A => Can[B]): Can[B] = Empty

  /**
  * @returns this Can if it has a value satisfying a predicate
  */
  def filter(p: A => Boolean): Can[A] = this

  /**
  * @returns true if the Can's value verifies a predicate
  */
  def exists(func: A => Boolean): Boolean = false

  /**
  * applies a function to the Can value
  */
  def foreach(f: A => Any): Unit = {}

  /**
  * If the contents of the Can isA instance of
  * the given class, return a Full[B], otherwise
  * Empty
  */
  def isA[B](cls: Class[B]): Can[B] = Empty

  /**
  * @returns a this or an alternative Can if this is an Empty Can
  */
  def or[B >: A](alternative: => Can[B]): Can[B] = alternative

  /**
  * @returns an iterator on the Can value
  */
  def elements: Iterator[A] = Iterator.empty

  /**
  * @returns true if the Can's value verifies a predicate
  */
  def toList: List[A] = Nil

  /**
  * @returns the Can as an Option
  */
  def toOption: Option[A] = None

  /**
  * @param msg the failure message
  * @returns a Failure with the message if the Can is an Empty Can
  */
  def ?~(msg: String): Can[A] = this

  /**
  * Alias for ?~
  */
  def failMsg(msg: String): Can[A] = ?~(msg)

  /**
  * @param msg the failure message
  * @returns a Failure with the message if the Can is an Empty Can. Chain the messages if it is already a Failure
  */
  def ?~!(msg: String): Can[A] = ?~(msg)

  /**
  * Alias for ?~!
  */
  def compoundFailMsg(msg: String): Can[A] = ?~!(msg)

  /**
  * @param msg the failure message
  * @param p a predicate
  * @returns a Failure with the message if the predicate is not satisfied with the Can's value
  */
  def filterMsg(msg: String)(p: A => Boolean): Can[A] = filter(p) ?~ msg

  /**
  * runs a function on the Can's value
  * @returns the result of the function or a default value
  */
  def run[T](in: T)(f: (T, A) => T) = in

  /**
  * pass the Can's value to a function
  * @returns the Can
  */
  def pass(f: Can[A] => Any) = {f(this) ; this}

  /**
  * Alias for pass
  */
  def $(f: Can[A] => Any) = pass(f)

  /**
  * overrides the equals method for Cans (For Full and Empty only. For Failure, the method is overriden again)
  */
  override def equals(other: Any): Boolean = (this, other) match {
    case (Full(x), Full(y)) => x == y
    case (Full(x), y) => x == y
    case (x, y: AnyRef) => x eq y
    case _ => false
  }

  /**
  * applies the function f1 if possible, return an alternative Can otherwise
  */
  def choice[B](f1: A => Can[B])(alternative: => Can[B]): Can[B] = this match {
    case Full(x) => f1(x)
    case _ => alternative
  }

  def ===[B >: A](to: B): Boolean = false
}

/**
* The Full Can is a Can containing a value.
* It provides adequate behavior to a Can for when a value is involved
*/
@serializable
final case class Full[+A](value: A) extends Can[A] {

  // constructor -- do not allow Full(null)
  value match {
    case v: AnyRef if v eq null => throw new NullPointerException("A Full Can[T] cannot be set to null")
    case _ =>
  }

  def isEmpty: Boolean = false

  def open_! : A = value

  override def openOr[B >: A](default: => B): B = value

  override def or[B >: A](alternative: => Can[B]): Can[B] = this

  override def exists(func: A => Boolean): Boolean = func(value)

  override def filter(p: A => Boolean): Can[A] = if (p(value)) this else Empty

  override def foreach(f: A => Any): Unit = f(value)

  override def map[B](f: A => B): Can[B] = Full(f(value))

  override def flatMap[B](f: A => Can[B]): Can[B] = f(value)

  override def elements: Iterator[A] = Iterator.fromValues(value)

  override def toList: List[A] = List(value)

  override def toOption: Option[A] = Some(value)

  override def run[T](in: T)(f: (T, A) => T) = f(in, value)

  /**
   * If the contents of the Can isA instance of
   * the given class, return a Full[B], otherwise
   * Empty
   */
  override def isA[B](cls: Class[B]): Can[B] = value match {
    case value: AnyRef =>
    if (cls.isAssignableFrom(value.getClass)) Full(value.asInstanceOf[B])
    else Empty
    case _ => Empty
  }

    override def ===[B >: A](to: B): Boolean = value == to
}

/**
* Singleton object representing an Empty Can
*/
@serializable
case object Empty extends EmptyCan[Nothing]

/**
* The Empty Can is a Can containing no value.
* It provides adequate behavior to a Can for when no value is involved
*/
@serializable
abstract class EmptyCan[+A] extends Can[A] {

  def isEmpty: Boolean = true

  def open_!  = throw new NullPointerException("Trying to open an empty can")

  override def openOr[B >: A](default: => B): B = default

  override def or[B >: A](alternative: => Can[B]): Can[B] = alternative

  override def filter(p: A => Boolean): Can[A] = this

  override def ?~(msg: String) = Failure(msg, Empty, Nil)
}

object Failure {
  def apply(msg: String) = new Failure(msg, Empty, Nil)
}

/**
* A Failure is an Empty Can having a failure message explaining the reason for being empty
* It can also optionally provide an exception or a chain of causes represented as a list of other Failure objects
*/
@serializable
case class Failure(msg: String, exception: Can[Throwable], chain: List[Failure]) extends EmptyCan[Nothing] {
  type A = Nothing

  override def ?~(msg: String) = this

  override def ?~!(msg: String) = Failure(msg, Empty, this :: chain)

  override def map[B](f: A => B): Can[B] = this

  override def flatMap[B](f: A => Can[B]): Can[B] = this

  /**
  * If the contents of the Can isA instance of
  * the given class, return a Full[B], otherwise
  * Empty
  */
  override def isA[B](cls: Class[B]): Can[B] = this

  def messageChain: String = (this :: chain).map(_.msg).mkString(" <- ")

  override def equals(other: Any): Boolean = (this, other) match {
    case (Failure(x, y, z), Failure(x1, y1, z1)) => (x, y, z) == (x1, y1, z1)
    case (x, y: AnyRef) => x eq y
    case _ => false
  }
}

// vim: set ts=2 sw=2 et:

