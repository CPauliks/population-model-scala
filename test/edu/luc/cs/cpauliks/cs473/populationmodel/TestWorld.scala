package edu.luc.cs.cpauliks.cs473.populationmodel

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{WordSpec, BeforeAndAfterAll}
import akka.actor.Actor._
import akka.util.Duration
import java.util.concurrent.TimeUnit
import akka.testkit._
import java.util.concurrent.TimeUnit
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import edu.luc.cs.cpauliks.cs473.populationmodel.SimulationMesages._

@RunWith(classOf[JUnitRunner])
class TestWorld extends WordSpec with ShouldMatchers with TestKit {
  
  val testWorld = actorOf(new World(10, 10, 1, 1, 20, 201, 5, 20, 40, 20)).start()
  
  "A World" should {
    "not reply to a Location update within it's boundaries" in {
      testWorld ! LynxReproduced(2,3, self)
      testWorld ! NewLynxLocation(2,3, 3, 4, self)
      expectNoMsg(Duration(100, "millis"))
    }
  }
  
  "A World" should {
    "send location corrections if a Location update is outside the map" in {
      testWorld ! LynxReproduced(2,3, self)
      testWorld ! NewLynxLocation(2,3,12,11, self)
      expectMsg(CorrectLocation(2,1))
    }
  }

}