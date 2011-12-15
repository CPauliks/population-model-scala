package edu.luc.cs.cpauliks.cs473.populationmodel

import akka.actor._
import akka.actor.Actor._
import edu.luc.cs.cpauliks.cs473.populationmodel.SimulationMesages._
import akka.testkit._
import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.MustMatchers

class TestHare extends WordSpec with MustMatchers with TestKit{
  val testHare = actorOf(new Hare(0,0,6)).start()
  
  "A Hare" must {
    "send back a reproduce request" in {
      testHare ! Reproduce;
      expectMsg(HareCanReproduce(0,0))
    }
  }
  
  "A Hare" must {
    "reply to a ping with a pong" in {
      testHare ! Ping
      expectMsg(HarePong)
    }
  }
  

}