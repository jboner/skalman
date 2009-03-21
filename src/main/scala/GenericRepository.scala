/**
 * Copyright (C) 2008 Scalable Solutions.
 */

package se.scalablesolutions.skalman

import javax.persistence.{EntityManager, NoResultException}
import java.util.{List => JList}

import javax.ejb.{TransactionAttribute, TransactionAttributeType}

import scala.collection.jcl.Conversions._

import ManagedComponentContext._
import JPA._
import util.Helpers._
import util.HashCode

// Exceptions
class CreateException(msg: String) extends RuntimeException(msg)
class UpdateException(msg: String) extends RuntimeException(msg)
class FindException(msg: String) extends RuntimeException(msg)
class DeleteException(msg: String) extends RuntimeException(msg)
class EntityExistsException(msg: String) extends RuntimeException(msg)
/**
 * Defines a ranged of results, to be used with f.e. listings etc.
 */
@serializable
class ResultRange(val first: Int, val limit: Int) {
  def this() = this(0, 0)

  def last = first + limit

  override def hashCode(): Int = {
    var result = HashCode.SEED
    result = HashCode.hash(result, first)
    result = HashCode.hash(result, limit)
    result
  }
  
  override def equals(that: Any) : Boolean = {
    that != null &&
    that.isInstanceOf[ResultRange] && {
      val other = that.asInstanceOf[ResultRange]
      other.first == first &&
      other.limit == limit
    }
  }
}

trait GenericRepository[T, PK] {
  val clazz: Class[T]

  @TransactionAttribute(TransactionAttributeType.REQUIRED)
  def create(entity: T): T = {
    try { persist(entity) } catch {
      case e: javax.persistence.EntityExistsException =>
        throw new EntityExistsException(e.getMessage())
    }
    entity
  }

  @TransactionAttribute(TransactionAttributeType.REQUIRED)
  def update(entity: T): T = merge(entity)

  @TransactionAttribute(TransactionAttributeType.REQUIRED)
  def delete(entity: T) = attachAndRemove(entity)

  def find(id: PK): T = withEntityManager { _.find(clazz, id).asInstanceOf[T] }
  def findAll: List[T] = withEntityManager { _.createQuery("from " + clazz.getSimpleName).getResultList.asInstanceOf[JList[T]].toList }
}
