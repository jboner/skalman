/**
 * Copyright (C) 2008 Triental AB.
 */

package com.triental.gps.container

import org.testng.annotations.{BeforeSuite, BeforeMethod, Test}
import org.testng.Assert._

import com.triental.gps.annotation.Cacheable

import org.scalatest.testng.TestNGSuite
import org.scalatest._

class CacheSuite extends TestNGSuite {

  val TIMEOUT = 1000 * 60 * 5
  var hit = ""
  val test = ManagedComponentFactory.createComponent[IFoo](
    classOf[IFoo],
    new ManagedComponentProxy(new FooImpl)
      with CachingInterceptor)

  trait IFoo extends ManagedComponent {
    @Cacheable def noargs: String
    @Cacheable def simpleArg(arg: String): String
    @Cacheable def complexArg(i: Int, arr: Array[Double], lst: List[String], arg: String): String
  }
  class FooImpl extends IFoo  {
    def noargs: String = {
      hit = "cache"
      "noargs"
    }
    def simpleArg(arg: String): String = {
      hit = "cache"
      "simpleArg"
    }
    def complexArg(i: Int, arr: Array[Double], lst: List[String], arg: String): String = {
      hit = "cache"
      "complexArg"
    }
  }

  @Test { val groups=Array("unit") }
  def testNoArgs = {
    hit = ""
    assert(test.noargs === "noargs")
    assert(hit === "cache")
    hit = ""
    assert(test.noargs === "noargs")
    assert(hit === "")
    hit = ""
    assert(test.noargs === "noargs")
    assert(hit === "")
    hit = ""
    assert(test.noargs === "noargs")
    assert(hit === "")
    hit = ""
    assert(test.noargs === "noargs")
    assert(hit === "")
    assert(true === true)
  }

  @Test { val groups=Array("unit") }
  def testSimpleArgs = {
    hit = ""
    assert(test.simpleArg("1") === "simpleArg")
    assert(hit === "cache")
    hit = ""
    assert(test.simpleArg("2") === "simpleArg")
    assert(hit === "cache")
    hit = ""
    assert(test.simpleArg("1") === "simpleArg")
    assert(hit === "")
    hit = ""
    assert(test.simpleArg("2") === "simpleArg")
    assert(hit === "")
    hit = ""
    assert(true === true)
  }

  @Test { val groups=Array("unit") }
  def testComplexArgs = {
    hit = ""
    assert(test.complexArg(1, Array(1.0D, 3.0D), List("one", "two", "three"), "bla") === "complexArg")
    assert(hit === "cache")
    hit = ""
    assert(test.complexArg(1, Array(1.0D, 3.0D), List("one", "two", "three"), "bla") === "complexArg")
    assert(hit === "")
    hit = ""
    assert(test.complexArg(1, Array(1.0D, 3.0D), List("one", "two", "three"), "bla") === "complexArg")
    assert(hit === "")
    hit = ""
    assert(test.complexArg(2, Array(1.0D, 3.0D), List("one", "two", "three"), "bla") === "complexArg")
    assert(hit === "cache")
    hit = ""
    assert(test.complexArg(2, Array(1.0D, 3.0D), List("one", "two", "three"), "bla") === "complexArg")
    assert(hit === "")
    hit = ""
    assert(test.complexArg(2, Array(3.0D), List("one", "three"), "bla") === "complexArg")
    assert(hit === "cache")
    hit = ""
    assert(test.complexArg(2, Array(3.0D), List("one", "three"), "bla") === "complexArg")
    assert(hit === "")
    hit = ""
    assert(test.complexArg(1, Array(1.0D, 3.0D), List("one", "two", "three"), "bla") === "complexArg")
    assert(hit === "")
    hit = ""
    assert(test.complexArg(2, Array(1.0D, 3.0D), List("one", "two", "three"), "bla") === "complexArg")
    assert(hit === "")
    hit = ""
    assert(true === true)
  }
}
