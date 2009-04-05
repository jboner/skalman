/**
 * Copyright (C) 2008 Scalable Solutions.
 */

package se.scalablesolutions.skalman

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.jcl.{Map, HashMap}
import scala.collection.mutable.{ArrayBuffer}
import scala.util.DynamicVariable

import java.lang.reflect.{Method, Field, InvocationHandler, Proxy, InvocationTargetException, UndeclaredThrowableException}
import javax.persistence.{EntityManager, EntityManagerFactory}
import javax.transaction.{Transaction, TransactionManager}
import java.lang.annotation.Annotation

import org.aspectj.weaver.tools.{PointcutParser, PointcutExpression, ShadowMatch}

import TransactionService._
import JPA._
import util.Helpers._
import util.HashCode
import util.{Can, Full, Empty, Failure}

/**
 * Service interface for all managed components.
 */
trait ManagedComponent extends Logging {
  log.info("Starting Managed Component [%s]", this.getClass.getName)
  private[this] var proxy: ManagedComponentProxy = _
  def init = {}
  def getProxy = proxy
  private[skalman] def setProxy(p: ManagedComponentProxy) = proxy = p
}

/**
 * Represents a snapshot of the current invocation.
 */
case class Invocation(val method: Method, val args: Array[Object], val target: AnyRef) {
  def invoke: AnyRef = method.invoke(target, args:_*)
  override def toString: String = "Invocation [method: " + method.getName + ", args: " + args + ", target: " + target + "]"
  override def hashCode(): Int = {
    var result = HashCode.SEED
    result = HashCode.hash(result, method)
    result = HashCode.hash(result, args)
    result = HashCode.hash(result, target)
    result
  }
  
  override def equals(that: Any): Boolean = {
    that != null &&
    that.isInstanceOf[Invocation] &&
    that.asInstanceOf[Invocation].method == method &&
    that.asInstanceOf[Invocation].target == target &&
    isEqual(that.asInstanceOf[Invocation].args, args)
  } 

  private def isEqual(a1: Array[Object], a2: Array[Object]): Boolean =
    (a1 == null && a2 == null) || (a1 != null && a2 != null && a1.size == a2.size && a1.zip(a2).find(t => t._1 == t._2).isDefined)  
}

/**
 * Base for all skalman interceptors.
 */
trait Interceptor extends Logging {
  protected val parser = PointcutParser.getPointcutParserSupportingAllPrimitivesAndUsingContextClassloaderForResolution

  protected def matches(pointcut: PointcutExpression, invocation: Invocation): Boolean = {
    pointcut.matchesMethodExecution(invocation.method).alwaysMatches ||
    invocation.target.getClass.getDeclaredMethods.exists(pointcut.matchesMethodExecution(_).alwaysMatches) ||
    false
  }

  protected def matches(annotationClass: Class[T] forSome {type T <: Annotation}, invocation: Invocation): Boolean = {
    invocation.method.isAnnotationPresent(annotationClass) ||
    invocation.target.getClass.isAnnotationPresent(annotationClass) ||
    invocation.target.getClass.getInterfaces.exists(_.isAnnotationPresent(annotationClass)) || // if one of the interfaces target implements has a matching annotation => match all methods in target - is this correct semantics???
    false
  }

  def invoke(invocation: Invocation): AnyRef
}

/**
 * The skalman context, holds the EntityManager and the TransactionManager.
 */
class ManagedComponentContext private (
  private var em: Option[EntityManager],
  private val tm: TransactionManager) {

  private def this() = this(None, getTransactionManager)
  private def this(em: EntityManager) = this(Some(em), getTransactionManager)
  private def this(tm: TransactionManager) = this(None, tm)

  /**
   * Returns the current TransactionManager.
   */
  private def getTransactionManager: TransactionManager = tm

  /**
   * Returns the current EntityManager.
   */
   private def getEntityManager: EntityManager = em match {
     case Some(entityManager) => entityManager
     case None => 
       val entityManager = getEntityManagerFactory.createEntityManager
       em = Some(entityManager)
       entityManager
   }

  /**
   * Closes and removes the current EntityManager.
   * <p/>
   * PLEASE NOTE: This method must always be used to close the EntityManager, never use em.close directly.
   */
  private def closeEntityManager = if (em.isDefined) {
    em.get.close
    em = None
  }
}

/**
 * Manages a thread-local stack of ManagedComponentContext's.
 */
object ManagedComponentContext extends Logging {
  private[ManagedComponentContext] val stack = new DynamicVariable(new ManagedComponentContext)

  /**
   * Returns the current TransactionManager.
   */
  def getTransactionManager: TransactionManager = current.getTransactionManager

  /**
   * Returns the current EntityManager.
   */
  def getEntityManager: EntityManager = current.getEntityManager

  /**
   * Checks if an EntityManager exists in current context.
   */
  def hasEntityManager: Boolean = current.em.isDefined

  /**
   * Closes and removes the current EntityManager.
   * <p/>
   * NOTE: This method must always be used to close the EntityManager, never use em.close directly.
   */
  def closeEntityManager = current.closeEntityManager

  /**
   * Returns the current context.
   */
  private def current = stack.value

  /**
   * Continues with the invocation defined in 'body' with the brand new context define in 'newCtx', the old
   * one is put on the stack and will automatically come back in scope when the method exits.
   */
  private[skalman] def withContext[T](body: => T): T = {
    stack.withValue(new ManagedComponentContext) { body }
  }
}

object ManagedComponentFactory {

  def createComponent[T](intf: Class[T] forSome {type T}, proxy: ManagedComponentProxy): T = {
    val component = Proxy.newProxyInstance(
      proxy.target.getClass.getClassLoader,
      Array(intf, classOf[ManagedComponent]),
      proxy)
    component.asInstanceOf[ManagedComponent].setProxy(proxy)
    component.asInstanceOf[T]
  }

  def createComponent[T, I](interfaces: Array[Class[I] forSome {type I}], proxy: ManagedComponentProxy): T = {
    val component = Proxy.newProxyInstance(
      proxy.target.getClass.getClassLoader,
      interfaces ++ Array(classOf[ManagedComponent]),
      proxy)
      component.asInstanceOf[ManagedComponent].setProxy(proxy)
      component.asInstanceOf[T]
  }

  def createActorBasedComponent[T](intf: Class[T] forSome {type T}, proxy: ManagedComponentProxy): T = {
    proxy.isActorBased = true
    val component = Proxy.newProxyInstance(
      proxy.target.getClass.getClassLoader,
      Array(intf, classOf[ManagedComponent]),
      proxy)
      component.asInstanceOf[ManagedComponent].setProxy(proxy)
      component.asInstanceOf[T]
  }

  def createActorBasedComponent[T](intf: Class[T] forSome {type T}, proxy: ManagedComponentProxy, dispatcher: Actor): T = {
    proxy.isActorBased = true
    proxy.dispatcher = dispatcher
    val component = Proxy.newProxyInstance(
      proxy.target.getClass.getClassLoader,
      Array(intf, classOf[ManagedComponent]),
      proxy)
      component.asInstanceOf[ManagedComponent].setProxy(proxy)
      component.asInstanceOf[T]
  }

  def createActorBasedComponent[T, R](interfaces: Array[Class[T] forSome {type T}], proxy: ManagedComponentProxy): R = {
    proxy.isActorBased = true
    val component = Proxy.newProxyInstance(
      proxy.target.getClass.getClassLoader,
      interfaces ++ Array(classOf[ManagedComponent]),
      proxy)
      component.asInstanceOf[ManagedComponent].setProxy(proxy)
      component.asInstanceOf[R]
  }

  def createActorBasedComponent[T, R](interfaces: Array[Class[T] forSome {type T}], proxy: ManagedComponentProxy, dispatcher: Actor): R = {
    proxy.isActorBased = true
    proxy.dispatcher = dispatcher
    val component = Proxy.newProxyInstance(
      proxy.target.getClass.getClassLoader,
      interfaces ++ Array(classOf[ManagedComponent]),
      proxy)
      component.asInstanceOf[ManagedComponent].setProxy(proxy)
      component.asInstanceOf[R]
  }
}

class ManagedComponentProxy(val target: AnyRef) extends InvocationHandler {
  private[skalman] var isActorBased = false

  private[skalman] var dispatcher =
    actor {
      loop {
        react {
          case invocation: Invocation =>
            try {
              reply(Full(invocation.invoke))
            } catch {
              case e: InvocationTargetException =>
                val cause = e.getTargetException
                Failure(cause.getMessage, Full(cause), Nil)
              case e =>
                Failure(e.getMessage, Full(e), Nil)
            }
          case 'exit =>  exit; reply(Empty)
          case unexpected => throw new IllegalArgumentException("Unexpected message to actor proxy: " + unexpected)
        }
      }
    }

  def invoke(proxy: AnyRef, m: Method, args: Array[AnyRef]): AnyRef = invoke(Invocation(m, args, target))

  def invoke(invocation: Invocation): AnyRef = {
    if (!isActorBased) {
      try {
        invocation.invoke
      } catch {
        case e: InvocationTargetException => throw e.getTargetException
        // For checked exceptions 
        case e: UndeclaredThrowableException => throw e.getCause
        case e => throw e
      }
    } else {
      val future = dispatcher !! (invocation, { case any => any.asInstanceOf[AnyRef] })
      future() match {
        case Full(result) => result.asInstanceOf[AnyRef]
        case Failure(message, Full(e), _) => throw e
        case Failure(message, Empty, _) => throw new RuntimeException(message)
        case Empty => {}
      }
    }
  }
}

// =============== SAMPLE CODE  ================

object ProxyTest extends Application {
import javax.ejb.{TransactionAttribute, TransactionAttributeType}

  trait Foo {
    @TransactionAttribute(TransactionAttributeType.REQUIRED)
    def foo(msg: String)
    def bar(msg: String)
  }

  class FooImpl extends Foo {
    val bar: Bar = new BarImpl
    def foo(msg: String) = println("msg: " + msg)
    def bar(msg: String) = bar.bar(msg)
  }

  trait Bar {
    def bar(msg: String)
  }

  class BarImpl extends Bar {
    def bar(msg: String) = println("msg: " + msg)
  }

  // Dummy Logging interceptor
  trait LoggingInterceptor extends Interceptor {
    val logPointcut = parser.parsePointcutExpression("execution(* *.foo(..))")
    abstract override def invoke(invocation: Invocation): AnyRef = if (matches(logPointcut, invocation)) {
      println("=====> Enter: " + invocation.method.getName + " @ " + invocation.target.getClass.getName)
      val result = super.invoke(invocation)
      println("=====> Exit: " + invocation.method.getName + " @ " + invocation.target.getClass.getName)
      result
    } else super.invoke(invocation)
  }

  // Dummy Transaction interceptor
  trait TransactionInterceptor extends Interceptor {
    val txPointcut = parser.parsePointcutExpression("execution(* *.bar(..))")
    abstract override def invoke(invocation: Invocation): AnyRef = if (matches(txPointcut, invocation)) {
      println("=====> TX begin")
      val result = super.invoke(invocation)
      println("=====> TX commit")
      result
    } else super.invoke(invocation)
  }

  // 1. ======================================================
  println("\n-------- CREATING NON ACTOR BASED COMPONENT ------")

  var foo = ManagedComponentFactory.createComponent[Foo](
    classOf[Foo],
    new ManagedComponentProxy(new FooImpl)
      with LoggingInterceptor
      with TransactionInterceptor)

  foo.foo("foo")
  foo.bar("bar")

  // 2. ======================================================
  println("\n-------- CREATING ACTOR BASED COMPONENT USING SAME INTERFACE AND IMPL ------")

  foo = ManagedComponentFactory.createActorBasedComponent[Foo](
    classOf[Foo],
    new ManagedComponentProxy(new FooImpl)
      with LoggingInterceptor
      with TransactionInterceptor)

  foo.foo("foo")
  foo.bar("bar")

  // 3. ======================================================
  println("\n-------- CREATING CUSTOM ACTOR BASED COMPONENT USING SAME INTERFACE AND IMPL ------")

  // Usually spawned up once and stored away somewhere
  val customActor = actor(loop(react({
    case invocation: Invocation => reply(Full(invocation.invoke))
    case 'exit => exit(); reply(Empty)
  })))

  foo = ManagedComponentFactory.createActorBasedComponent[Foo](
    classOf[Foo],
    new ManagedComponentProxy(new FooImpl)
      with LoggingInterceptor
      with TransactionInterceptor,
    customActor)

  foo.foo("foo")
  foo.bar("bar")

  // to close down in an elegant way
  customActor ! 'exit
  foo = null
  Runtime.getRuntime.gc()
}
