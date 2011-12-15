package edu.luc.cs.cpauliks.cs473.populationmodel

import akka.actor._
import akka.actor.Actor._
import edu.luc.cs.cpauliks.cs473.populationmodel.SimulationMesages._

object PopulationModel extends App {
  
	/**
	 * Values for the simulation
	 * Uses values from the NetLogo Tutorial
	 */
	private val worldSizeX = 50 //Not in NL Tutorial, but needed here
	private val worldSizeY = 50
	private val initialHares = 100 //Recommended 0-1000
	private val initialLynx = 20 //Recommended 0-100
	private val hareBirthRate = 35 //Recommended 0-200
	private val maxHareBirthRate = 201
	private val maxHareAge = 6 //Recommended 0-20
	private val lynxEnergyToReproduce = 30 //Recommended 0-100
	private val energyPerHareEaten = 10 //Recommended 0-30
	private val maxLynxAge = 20 //Recommended 0-50
	  
	val world = actorOf(new World(worldSizeX, worldSizeY, initialHares, initialLynx, hareBirthRate, maxHareBirthRate, maxHareAge, lynxEnergyToReproduce, energyPerHareEaten, maxLynxAge))
	world.start()
	world ! Start
  
}

object SimulationMesages {
    /**
   * Messages passed between the World and the Animals
   */
  trait MessageFromAnimal
  trait MessageToAnimal
  
  //Hare
  case class NewHareLocation(oldX: Int, oldY: Int, newX: Int, newY: Int, hare: ActorRef) extends MessageFromAnimal
  case class HareCanReproduce(xPos: Int, yPos: Int) extends MessageFromAnimal
  case class HareDied(xPos: Int, yPos: Int, hare: ActorRef) extends MessageFromAnimal
  case class HarePong(hare: ActorRef) extends MessageFromAnimal
  
  //Lynx
  case class NewLynxLocation(oldX: Int, oldY: Int, newX: Int, newY: Int, lynx: ActorRef) extends MessageFromAnimal
  case class LynxReproduced(xPos: Int, yPos: Int, newLynx: ActorRef) extends MessageFromAnimal
  case class LynxDied(xPos: Int, yPos: Int, lyxn: ActorRef) extends MessageFromAnimal
  case class LynxPong(lynx: ActorRef) extends MessageFromAnimal
  
  //Actions
  case object Move extends MessageToAnimal
  case object Ping extends MessageToAnimal
  case object Eat extends MessageToAnimal 
  case object Reproduce extends MessageToAnimal
  case object Age extends MessageToAnimal
  case class CorrectLocation(xPos: Int, yPos: Int) extends MessageToAnimal
  
 //Starts the simulation
  case object Start  
}
