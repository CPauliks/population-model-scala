package edu.luc.cs.cpauliks.cs473.populationmodel

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._
import akka.testkit.TestActorRef
import edu.luc.cs.cpauliks.cs473.populationmodel.SimulationMesages._

@RunWith(classOf[JUnitRunner])
class TestLynxInternals extends FunSuite {
  val maxAge = 5
  val startingEnergy = 5
  val energyPerHare = 10
  val energyToReproduce = 10
  val lynx = TestActorRef(new Lynx(0,0, maxAge, startingEnergy, energyPerHare, energyToReproduce)).start()
  val lynxActor = lynx.underlyingActor
  
  test("Lynx does not consider itself dead until it is at it's max age or out of energy") {
    assertEquals(true, lynxActor.checkIfStillAlive())
    lynxActor.age = maxAge+1
    assertEquals(false, lynxActor.checkIfStillAlive())
    lynxActor.age = maxAge
    assertEquals(true, lynxActor.checkIfStillAlive())
    lynxActor.energy = 0
    assertEquals(false, lynxActor.checkIfStillAlive())
  }
  
  test("Lynx can reproduce with enough energy") {
    assertEquals(false, lynxActor.canReproduce())
    lynx ! Eat
    assertEquals(true, lynxActor.canReproduce())
  }
  
  test("Lynx loses half it's energy upon reproducing") {
    lynx ! Eat
    val currentEnergy = lynxActor.energy - 1
    lynx ! Reproduce
    assertEquals(currentEnergy/2, lynxActor.energy)
  }

}