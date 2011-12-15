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
import akka.testkit.TestActorRef
import edu.luc.cs.cpauliks.cs473.populationmodel.SimulationMesages._

@RunWith(classOf[JUnitRunner])
class TestHare extends WordSpec with ShouldMatchers with TestKit {

  val yearsToDeath = 6
   val testHare = actorOf(new Hare(0,0,yearsToDeath)).start()
 
  "A Hare" should {
    "Reply to a Ping with a Pong" in {
      testHare ! Ping
      expectMsg(HarePong(testHare))
    }
  }
   
   "A Hare" should {
     "reply to a reproduce request" in {
       testHare ! Reproduce
       expectMsg(HareCanReproduce(0,0))
    }
  }
   
   "A Hare" should {
     "reply to a move with a location update" in {
       testHare ! Move
       expectMsgClass(classOf[NewHareLocation])
    }
  }
   


}