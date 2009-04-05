/**
 * Copyright (C) 2008 Scalable Solutions.
 */

package se.scalablesolutions.skalman

import javax.persistence.{
  Persistence => JPersistence,
  EntityManager,
  EntityManagerFactory,
  PersistenceException,
  Query,
  NoResultException,
  NonUniqueResultException
}
import javax.transaction.{TransactionManager, Status}
import java.util.{List => JList}
import scala.collection.jcl.Conversions._

/**
 * Function repository for EntityManager/JPA utility functions.
 */
object JPA {

  import ManagedComponentContext._

  // The primary EntityManagerFactory using context defined in persistence.xml
  private[JPA] lazy val entityManagerFactory: EntityManagerFactory =
    JPersistence.createEntityManagerFactory("SkalmanPersistenceUnit")

  /**
   * Returns the current EntityManagerFactory
   */
  def getEntityManagerFactory = entityManagerFactory

  //-------------------------------------------------------------------------
  // Convenience EntityManager management functions
  //-------------------------------------------------------------------------

  def withEntityManager[T](body: EntityManager => T): T = body(getEntityManager)

  def withEntityManagerTransaction[T](body: => T): T = {
    try {
      getEntityManager.getTransaction.begin
      val result = body
      getEntityManager.getTransaction.commit
      result
    } catch {
      case e =>
        if (!e.isInstanceOf[NoResultException] && // according the EJB3 spec we should not rollback on these exceptions
            !e.isInstanceOf[NonUniqueResultException]) {
          log.debug("Rolling back transaction due to: %s", e.toString)
          getEntityManager.getTransaction.rollback
        } else getEntityManager.getTransaction.commit
      throw e
    }
  }
  //-------------------------------------------------------------------------
  // Convenience methods for load, save, delete
  //-------------------------------------------------------------------------
  def persist[T](entity: T) = withEntityManager { _.persist(entity) }

  def merge[T](entity: T): T = withEntityManager { _.merge(entity).asInstanceOf[T] }

  def remove[T](entity: T) = withEntityManager { _.remove(entity) }

  def attachAndRemove[T](entity: T) = withEntityManager { _.remove(getEntityManager.merge(entity)) }

  def getReference[T](entityClass: Class[T], id: T): T = withEntityManager { _.getReference(entityClass, id).asInstanceOf[T] }

  def contains[T](entity: T) = withEntityManager { _.contains(entity) }

  def refresh[T](entity: T) = withEntityManager { _.refresh(entity) }

  def flush = withEntityManager { _.flush }

  //-------------------------------------------------------------------------
  // Convenience finder methods
  //-------------------------------------------------------------------------
  def find[T, I](entityClass: Class[T], id: I) = withEntityManager { _.find(entityClass, id) }

  def find[T](entityClass: Class[T], id: Int) = withEntityManager { _.find(entityClass, id) }

  def find[T](queryString: String, values: Any*): List[T] = withEntityManager { em => 
    val query = em.createQuery(queryString)
    if (values != null) {
      for (i <- 0 until values.size) {
        query.setParameter(i + 1, values(i))
      }
    }
    query.getResultList.asInstanceOf[JList[T]].toList
  }

  def findByNamedQuery[T](queryName: String, values: Any*): List[T] = withEntityManager { em =>
    val query = em.createNamedQuery(queryName)
    if (values != null) {
      for (i <- 0 until values.size) {
        query.setParameter(i + 1, values(i))
      }
    }
    query.getResultList.asInstanceOf[JList[T]].toList
  }

  def findByNamedQueryAndRange[T](queryName: String, resultRange: ResultRange): List[T] = withEntityManager {
    _.createNamedQuery(queryName).
                     setFirstResult(resultRange.first).
                     setMaxResults(resultRange.last).
                     getResultList.
                     asInstanceOf[JList[T]].toList
  }

  def findByNamedParams[T](queryString: String, params: Map[String, Any]): List[T] = withEntityManager { em =>
    val query = em.createQuery(queryString)
    if (params != null) {
      for ((key, value) <- params) {
        query.setParameter(key, value)
      }
    }
    query.getResultList.asInstanceOf[JList[T]].toList
  }

  def findByNamedQueryAndNamedParams[T](queryName: String, params: Map[String, Any]): List[T] = withEntityManager { em =>
    val query = em.createNamedQuery(queryName)
    if (params != null) {
      for ((key, value) <- params) {
        query.setParameter(key, value)
      }
    }
    query.getResultList.asInstanceOf[JList[T]].toList
  }

  def findByNamedQueryAndNamedParamsAndRange[T](queryName: String, params: Map[String, Any], resultRange: ResultRange): List[T] = withEntityManager { em =>
    val query = em.createNamedQuery(queryName)
    if (params != null) {
      for ((key, value) <- params) {
        query.setParameter(key, value)
      }
    }
    query.setFirstResult(resultRange.first).
          setMaxResults(resultRange.last).
          getResultList.
          asInstanceOf[JList[T]].toList
  }
}
