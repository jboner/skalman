/**
 * Copyright (C) 2008 Triental AB.
 */

package com.triental.gps.container

import org.scalatest._
import org.scalatest.testng.TestNGSuite

import com.triental.gps.container._
import com.triental.gps.util.Helpers._
import com.triental.gps.test.TestEnvironment

import org.testng.annotations.{
  BeforeSuite,
  BeforeMethod,
  AfterMethod,
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

class ManagedComponentContextSuite extends TestNGSuite with TestEnvironment {
  var status = ""

  trait LevelOne {
    def em
    def tm
    def emLevelTwo
    def tmLevelTwo
  }
  class LevelOneImpl extends LevelOne {
    val levelTwo: LevelTwo = new LevelTwoImpl
    def em = status += ManagedComponentContext.getEntityManager.toString
    def tm = status += ManagedComponentContext.getTransactionManager.toString
    def emLevelTwo = { status += ManagedComponentContext.getEntityManager.toString; levelTwo.em }
    def tmLevelTwo = { status += ManagedComponentContext.getTransactionManager.toString; levelTwo.tm }
  }
  trait LevelTwo {
    def em
    def tm
  }
  class LevelTwoImpl extends LevelTwo {
    def em = status += ManagedComponentContext.getEntityManager.toString
    def tm = status += ManagedComponentContext.getTransactionManager.toString
  }

  @BeforeMethod { val groups = Array("dbunit") }
  def setup = {
    status = ""
  }

  @Test { val groups=Array("unit") }
  def testEntityManagerPropagationOneLevel = {
    val em = ManagedComponentContext.getEntityManager.toString
    val levelOne = new LevelOneImpl
    levelOne.em
    assert(em === status)
  }

  @Test { val groups=Array("unit") }
  def testTransactionManagerPropagationOneLevel = {
    val tm = ManagedComponentContext.getTransactionManager.toString
    val levelOne = new LevelOneImpl
    levelOne.tm
    assert(tm === status)
  }

  @Test { val groups=Array("unit") }
  def testEntityManagerPropagationTwoLevels = {
    val em = ManagedComponentContext.getEntityManager.toString
    val levelOne = new LevelOneImpl
    levelOne.emLevelTwo
    assert(em + em === status)
  }

  @Test { val groups=Array("unit") }
  def testTransactionManagerPropagationTwoLevels = {
    val tm = ManagedComponentContext.getTransactionManager.toString
    val levelOne = new LevelOneImpl
    levelOne.tmLevelTwo
    assert(tm + tm === status)
  }
}

