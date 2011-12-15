package edu.luc.cs.cpauliks.cs473.populationmodel

import akka.actor._
import akka.actor.Actor._
import edu.luc.cs.cpauliks.cs473.populationmodel.SimulationMesages._
import akka.testkit.TestKit
import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.MustMatchers

class TestLynx extends WordSpec with MustMatchers with TestKit {
  val testLynx = actorOf(new Lynx(0,0,0,0,0,0)).start()
}