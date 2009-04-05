/**
 * Copyright (C) 2008 Scalable Solutions.
 */

package se.scalablesolutions.skalman

import org.scalatest._
import org.scalatest.testng.TestNGSuite

import se.scalablesolutions.skalman.util.Helpers._

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

import java.text.DateFormat
import java.util.{Locale, Date, List => JList, ArrayList => JArrayList}
import javax.ejb.{TransactionAttribute, TransactionAttributeType}

import org.aspectj.weaver.tools.{PointcutParser, PointcutExpression, ShadowMatch}

class InterceptorSuite extends TestNGSuite {

  val parser = PointcutParser.getPointcutParserSupportingAllPrimitivesAndUsingContextClassloaderForResolution

  var status = ""
  @BeforeMethod { val groups = Array("unit") }
  def resetLog = status = ""

  @Test { val groups=Array("unit") }
  def testMethodAnnotationOnInterfaceMatchingInterceptor = {
    var foo = ManagedComponentFactory.createComponent[Foo](
      classOf[Foo],
      new ManagedComponentProxy(new FooImpl) with AnnotationMatchingInterceptor)

    foo.foo("foo ")
    assert(status === "before-annotation-matching foo after-annotation-matching ")
  }

  @Test { val groups=Array("unit") }
  def testMethodAnnotationOnParentInterfaceMatchingInterceptor = {
    var raz = ManagedComponentFactory.createComponent[Raz](
      classOf[Raz],
      new ManagedComponentProxy(new RazImpl) with AnnotationMatchingInterceptor)

    raz.foo("foo ")
    assert(status === "before-annotation-matching foo after-annotation-matching ")
  }

  @Test { val groups=Array("broken") }
  def testClassAnnotationOnInterfaceMatchingInterceptor = {
    var bar = ManagedComponentFactory.createComponent[Bar](
      classOf[Bar],
      new ManagedComponentProxy(new BarImpl) with AnnotationMatchingInterceptor)

    bar.bar("bar ")
    assert(status === "before-annotation-matching bar after-annotation-matching ")
  }

  @Test { val groups=Array("broken") }
  def testClassAnnotationOnImplMatchingInterceptor = {
    var baz = ManagedComponentFactory.createComponent[Baz](
      classOf[Baz],
      new ManagedComponentProxy(new BazImpl) with AnnotationMatchingInterceptor)

    baz.baz("baz ")
    assert(status === "before-annotation-matching baz after-annotation-matching ")
  }

  @Test { val groups=Array("broken") }
  def testPointcutMatchingInterceptor1 = {
    trait PointcutSpecImpl extends PointcutSpec{
      val pointcut = parser.parsePointcutExpression("execution(* *.foo(..))")
    }

    var foo = ManagedComponentFactory.createComponent[Foo](
      classOf[Foo],
      new ManagedComponentProxy(new FooImpl) with PointcutMatchingInterceptor with PointcutSpecImpl)

    foo.foo("foo ")
    assert(status === "before-pointcut-matching foo after-pointcut-matching ")
  }

  @Test { val groups=Array("broken") }
  def testPointcutMatchingInterceptor2 = {
    trait PointcutSpecImpl extends PointcutSpec{
      val pointcut = parser.parsePointcutExpression("execution(* *..FooImpl.f*(..))")
    }

    var foo = ManagedComponentFactory.createComponent[Foo](
      classOf[Foo],
      new ManagedComponentProxy(new FooImpl) with PointcutMatchingInterceptor with PointcutSpecImpl)

    foo.foo("foo ")
    assert(status === "before-pointcut-matching foo after-pointcut-matching ")
  }

  @Test { val groups=Array("broken") }
  def testPointcutMatchingInterceptor3 = {
    trait PointcutSpecImpl extends PointcutSpec{
      val pointcut = parser.parsePointcutExpression("execution(* *.f*(..))")
    }

    var foo = ManagedComponentFactory.createComponent[Foo](
      classOf[Foo],
      new ManagedComponentProxy(new FooImpl) with PointcutMatchingInterceptor with PointcutSpecImpl)

    foo.foo("foo ")
    assert(status === "before-pointcut-matching foo after-pointcut-matching ")
  }

  @Test { val groups=Array("broken") }
  def testPointcutMatchingInterceptor4 = {
    trait PointcutSpecImpl extends PointcutSpec{
      val pointcut = parser.parsePointcutExpression("execution(* *.*(String))")
    }

    var foo = ManagedComponentFactory.createComponent[Foo](
      classOf[Foo],
      new ManagedComponentProxy(new FooImpl) with PointcutMatchingInterceptor with PointcutSpecImpl)

    foo.foo("foo ")
    assert(status === "before-pointcut-matching foo after-pointcut-matching ")
  }

  @Test { val groups=Array("broken") }
  def testPointcutMatchingInterceptor5 = {
    trait PointcutSpecImpl extends PointcutSpec{
      val pointcut = parser.parsePointcutExpression("execution(* *.foo())")
    }

    var foo = ManagedComponentFactory.createComponent[Foo](
      classOf[Foo],
      new ManagedComponentProxy(new FooImpl) with PointcutMatchingInterceptor with PointcutSpecImpl)

    foo.foo("foo ")
    assert(status === "foo ")
  }

  @Test { val groups=Array("broken") }
  def testPointcutMatchingInterceptor6 = {
    trait PointcutSpecImpl extends PointcutSpec{
      val pointcut = parser.parsePointcutExpression("execution(* *..Foo+.foo(..))")
    }

    var foo = ManagedComponentFactory.createComponent[Foo](
      classOf[Foo],
      new ManagedComponentProxy(new FooImpl) with PointcutMatchingInterceptor with PointcutSpecImpl)

    foo.foo("foo ")
    assert(status === "before-pointcut-matching foo after-pointcut-matching ")
  }

  @Test { val groups=Array("broken") }
  def testPointcutMatchingInterceptor7 = {
    trait PointcutSpecImpl extends PointcutSpec{
      val pointcut = parser.parsePointcutExpression("execution(* *..Foo.f*(..))")
     }

    var foo = ManagedComponentFactory.createComponent[Foo](
      classOf[Foo],
      new ManagedComponentProxy(new FooImpl) with PointcutMatchingInterceptor with PointcutSpecImpl)

    foo.foo("foo ")
    assert(status === "before-pointcut-matching foo after-pointcut-matching ")
  }

  @Test { val groups=Array("unit") }
  def testPointcutMatchingInterceptor8 = {
    intercept(classOf[IllegalArgumentException]) {
      parser.parsePointcutExpression("execution(* FooImpl.f*(..))")
    }
  }

  // ====================
  trait Foo extends ManagedComponent {
    @TransactionAttribute(TransactionAttributeType.REQUIRED)
    def foo(msg: String)
    def bar(msg: String)
  }

  class FooImpl extends Foo {
    val bar: Bar = new BarImpl
    def foo(msg: String) = status += msg
    def bar(msg: String) = bar.bar(msg)
  }

  // ====================
  trait Bar extends ManagedComponent {
    @TransactionAttribute(TransactionAttributeType.REQUIRED)
    def bar(msg: String)
  }

  class BarImpl extends Bar {
    def bar(msg: String) = status += msg
  }

  // ====================
  trait Faz extends ManagedComponent {
    def faz(msg: String)
  }

  class FazImpl extends Faz {
    @TransactionAttribute(TransactionAttributeType.REQUIRED)
    def faz(msg: String) = status += msg
  }

  // ====================
  trait Baz extends ManagedComponent {
    def baz(msg: String)
  }

  class BazImpl extends Baz {
    @TransactionAttribute(TransactionAttributeType.REQUIRED)
    def baz(msg: String) = status += msg
  }

  trait Raz extends Foo {
    def raz(msg: String)
  }

  class RazImpl extends FooImpl with Raz {
    def raz(msg: String) = status += msg
  }

  // ====================
  trait AnnotationMatchingInterceptor extends Interceptor {
    val logPointcut = parser.parsePointcutExpression("execution(* *.foo(..))")
    val annotation = classOf[TransactionAttribute]
    abstract override def invoke(invocation: Invocation): AnyRef = if (matches(annotation, invocation)) {
      status += "before-annotation-matching "
      val result = super.invoke(invocation)
      status += "after-annotation-matching "
      result
    } else super.invoke(invocation)
  }

  // ====================
  trait PointcutSpec {
    val pointcut: PointcutExpression
  }

  trait PointcutMatchingInterceptor extends Interceptor { this: PointcutSpec =>
    abstract override def invoke(invocation: Invocation): AnyRef = if (matches(pointcut, invocation)) {
      status += "before-pointcut-matching "
      val result = super.invoke(invocation)
      status += "after-pointcut-matching "
      result
    } else super.invoke(invocation)
  }
}

