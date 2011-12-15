package edu.luc.cs.cpauliks.cs473.populationmodel

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._
import akka.testkit.TestActorRef
import edu.luc.cs.cpauliks.cs473.populationmodel.SimulationMesages._

@RunWith(classOf[JUnitRunner])
class TestHareInternals extends FunSuite {
  val maxAge = 5
  val hare = TestActorRef(new Hare(0,0, maxAge)).underlyingActor

  test("Hare does not consider itself dead until it is at its max age") {
    assertEquals(true, hare.checkIfStillAlive())
    hare.age = maxAge+1
    assertEquals(false, hare.checkIfStillAlive())
  }
}