/**
 * Copyright (C) 2008 Scalable Solutions.
 */

package se.scalablesolutions.skalman

import java.util.concurrent.{ConcurrentHashMap, Future, FutureTask, Callable, CancellationException, ExecutionException}
import java.lang.reflect.{Method, Proxy, InvocationHandler, InvocationTargetException}
import java.lang.System.{currentTimeMillis => timeNow}
import se.scalablesolutions.skalman.annotation.Cacheable

/**
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
class CacheFactory[ID] {
  private val caches = new ConcurrentHashMap[ID, AnyRef]

  def cacheFor(cacheId: ID) = caches.get(cacheId)

  /**
   * Usage:
   *
   * <pre>
   * // create a cache factory with its caches mapped to string ids
   * val factory = new CacheFactory[String]
   *
   * // creating a cache proxy ('foo') for the interface 'Foo' with implementation 'FooImpl'
   * val foo = factory.newInstance[Foo]("cacheID", classOf[Foo], new FooImpl, CACHE_EVICTION_TIMEOUT)
   *
   * // now 'foo' can be used just as any instance of FooImpl and will be cached automatically in the most effective way humanly possible...
   * foo.dostuff(...)
   * </pre>
   *
   * Note: If invoked with the same cacheId as in a prior run, then the old cache proxy instance is returned.
   */
  def newInstance[T <: AnyRef](cacheId: ID, intf: Class[T] forSome {type T}, target: AnyRef, timeout: Long): T = {
    val newProxy = ManagedComponentFactory.createComponent[T](intf, new ManagedComponentProxy(target) with CachingInterceptor)
    newProxy.asInstanceOf[CachingInterceptor].timeout = timeout
    val oldProxy = caches.putIfAbsent(cacheId, newProxy)
    if (oldProxy == null) newProxy.asInstanceOf[T]
    else oldProxy.asInstanceOf[T]
  }
}

private[skalman] class EvictableFuture(val future: FutureTask[AnyRef], val timeout: Long) {
  val timestamp: Long = timeNow
  def evict_? = timeNow - timestamp > timeout
}

/**
 * Caching interceptor which caches the result of methods by a hash of the method arguments.
 * <p/>
 * Is applied to methods annotated with the <code>com.triental.gps.caching.Cacheable</code> annotation,
 * to override this behavior, subclass the class and override the <code>matchingAnnotation</code> val.
 * <p/>
 * This interceptor is fully thread-safe and performant (based on a ConcurrentHashMap).
 * Target method invocations are wrapped in a Future and subsequent invocations with the same parameter list
 * made before the target invocations has completed will wait on the Future in progress.
 * <p/>
 * The cache is evictable and the eviction timeout is set in the <code>CacheFactory.newInstance</code> factory.
 */
trait CachingInterceptor extends Interceptor {

  protected val matchingAnnotation = classOf[Cacheable]

  protected val cache = new ConcurrentHashMap[Invocation, EvictableFuture]
  protected[skalman] var timeout: Long = -1L

  def clearCache = cache.clear
    
  abstract override def invoke(invocation: Invocation): AnyRef = if (matches(matchingAnnotation, invocation)) {
    while (true) {
      var currentFuture = cache.get(invocation)
      if (currentFuture == null || (timeout > 0 && currentFuture.evict_?)) { // TODO: perhaps move eviction to a separate scheduled thread (actor)
        val newFuture = new EvictableFuture(
          new FutureTask[AnyRef](
            new Callable[AnyRef] {
              def call: AnyRef = invocation.invoke
            }), timeout)
        cache.put(invocation, newFuture)
        currentFuture = newFuture
        newFuture.future.run
      }
      try { return currentFuture.future.get.asInstanceOf[Object] } catch {
        case e: CancellationException => cache.remove(invocation, currentFuture)
        case e: ExecutionException => throw e.getCause
      }
    }
  } else super.invoke(invocation)
}


