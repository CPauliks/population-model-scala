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
class TestLynx extends WordSpec with ShouldMatchers with TestKit {
  
   val yearsToDeath = 5
   val testLynx = actorOf(new Lynx(0,0,yearsToDeath,10,40,20)).start()
   
 "A Lynx" should {
    "Reply to a Ping with a Pong" in {
      testLynx ! Ping
      expectMsg(LynxPong(testLynx))
    }
   }
  
   "A Lynx" should {
     "reply to a move with a location update" in {
       testLynx ! Move
       expectMsgClass(classOf[NewLynxLocation])
    }
   }
   
    "A Lynx" should {
     "reply to a reproduce request only if it has the energy" in {
       testLynx ! Reproduce
       expectNoMsg(Duration(100, "millis"))
       testLynx ! Eat
       expectNoMsg(Duration(100, "millis"))
       testLynx ! Reproduce
       expectMsgClass(classOf[LynxReproduced])
    }
   }
}