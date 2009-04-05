/**
 * Copyright (C) 2008 Scalable Solutions.
 */

package se.scalablesolutions.skalman

import org.scalatest._
import org.scalatest.testng.TestNGSuite

import se.scalablesolutions.skalman.util.Helpers._
import se.scalablesolutions.skalman.util.{Can, Full, Empty, Failure}

import org.testng.annotations.{
  BeforeSuite,
  BeforeMethod,
  Test
}

import org.easymock.EasyMock.{
  expect => exp,
  createMock,
  verify,
  replay,
  reset,
  notNull,
  isA
}

import javax.ejb.{TransactionAttribute, TransactionAttributeType}

import scala.actors.Actor
import scala.actors.Actor._

// FIXME: Turn into real test, now there are no asserts just print outs
class ManagedComponentProxySuite extends TestNGSuite with Logging {

  var status = ""

  trait Foo extends ManagedComponent {
    @TransactionAttribute(TransactionAttributeType.REQUIRED)
    def foo(msg: String)
    def bar(msg: String)
  }

  class FooImpl extends Foo {
    val bar: Bar = new BarImpl // is normally injected by Guice
    def foo(msg: String) = log.info("msg: " + msg)
    def bar(msg: String) = bar.bar(msg)
  }

  trait Bar extends ManagedComponent {
    def bar(msg: String)
  }

  class BarImpl extends Bar {
    def bar(msg: String) = log.info("msg: " + msg)
  }

  // Logging interceptor
  trait LoggingInterceptor extends Interceptor {
    val logPointcut = parser.parsePointcutExpression("execution(* *.foo(..))")
    abstract override def invoke(invocation: Invocation): AnyRef = if (matches(logPointcut, invocation)) {
      log.info("=====> Enter: " + invocation.method.getName + " @ " + invocation.target.getClass.getName)
      val result = super.invoke(invocation)
      log.info("=====> Exit: " + invocation.method.getName + " @ " + invocation.target.getClass.getName)
      result
    } else super.invoke(invocation)
  }

  // Transaction interceptor
  trait TransactionInterceptor extends Interceptor {
    val txPointcut = parser.parsePointcutExpression("execution(* *.bar(..))")
    abstract override def invoke(invocation: Invocation): AnyRef = if (matches(txPointcut, invocation)) {
      log.info("=====> TX begin")
      val result = super.invoke(invocation)
      log.info("=====> TX commit")
      result
    } else super.invoke(invocation)
  }

  @BeforeMethod { val groups = Array("unit") }
  def resetLog = status = ""

  @Test { val groups=Array("unit") }
  def testCreateNonActorBasedComponent = {
    log.info("\n-------- CREATING NON ACTOR BASED COMPONENT ------")

    val foo = ManagedComponentFactory.createComponent[Foo](
      classOf[Foo],
      new ManagedComponentProxy(new FooImpl)
        with LoggingInterceptor
        with TransactionInterceptor)

    foo.foo("foo")
    foo.bar("bar")

    assert(true === true)
  }

  @Test { val groups=Array("unit") }
  def testCreateActorBasedComponent = {
    log.info("\n-------- CREATING ACTOR BASED COMPONENT USING SAME INTERFACE AND IMPL ------")

    val foo = ManagedComponentFactory.createComponent[Foo](
      classOf[Foo],
      new ManagedComponentProxy(new FooImpl)
        with LoggingInterceptor
        with TransactionInterceptor)

    foo.foo("foo")
    foo.bar("bar")

    assert(true === true)
  }

  @Test { val groups=Array("unit") }
  def testCreateCustomActorBasedComponent = {
    log.info("\n-------- CREATING CUSTOM ACTOR BASED COMPONENT USING SAME INTERFACE AND IMPL ------")

    // Usually spawned up once and stored away somewhere
    val customActor = actor(loop(react({
      case invocation: Invocation => reply(Full(invocation.invoke))
      case 'exit => exit(); reply(Empty)
    })))

    val foo = ManagedComponentFactory.createActorBasedComponent[Foo](
      classOf[Foo],
      new ManagedComponentProxy(new FooImpl)
      with LoggingInterceptor
      with TransactionInterceptor,
      customActor)

    foo.foo("foo")
    foo.bar("bar")

    // to close down in an elegant way
    customActor ! 'exit
    Runtime.getRuntime.gc()
    assert(true === true)
  }
}
