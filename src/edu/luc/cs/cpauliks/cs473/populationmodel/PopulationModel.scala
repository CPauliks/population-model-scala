package edu.luc.cs.cpauliks.cs473.populationmodel

import akka.actor._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object PopulationModel extends App {
  
  /**
   * Messages passed between the World and the Animals
   */
  trait MessageFromAnimal
  trait MessageToAnimal
  
  case class NewHareLocation(xPos: Int, yPos: Int, hare: ActorRef) extends MessageFromAnimal
  case class HareCanReproduce(xPos: Int, yPos: Int) extends MessageFromAnimal
  case class HareDied(xPos: Int, yPos: Int, hare: ActorRef) extends MessageFromAnimal
  
  case class LynxReproduced(newLynx: ActorRef) extends MessageFromAnimal
  case class LynxLookingForHare(xPos: Int, yPos: Int) extends MessageFromAnimal
  case object LynxDied extends MessageFromAnimal
  
  case class HarePong(hare: ActorRef) extends MessageFromAnimal
  case class LynxPong(lynx: ActorRef) extends MessageFromAnimal
  
  case object Move extends MessageToAnimal
  case object Ping extends MessageToAnimal
  case object Act extends MessageToAnimal
  
  abstract class Animal(xPosition: Int, yPosition: Int, maxAge: Int) extends Actor{
    private val rng = new Random()
    protected var age: Int = _
    protected var xPos = xPosition
    protected var yPos = yPosition
    
    def changeLocation = {
      val forwardX = rng.nextBoolean()
      val forwardY = rng.nextBoolean()
      xPos += (if(forwardX) rng.nextInt(2) else (rng.nextInt(2) * -1))
      yPos += (if(forwardX) rng.nextInt(2) else (rng.nextInt(2) * -1))
    }
  }
  
  class Hare(xPosition: Int, yPosition: Int, maxAge: Int) extends Animal(xPosition, yPosition, maxAge) {
    def receive = {
      case Move => {
        age += 1
        changeLocation
        self reply NewHareLocation(xPos, yPos, self)
      }
      
    }
  }
  
  class Lynx(xPosition: Int, yPosition: Int, maxAge: Int, startingEnergy: Int, energyPerHare: Int, energyToReproduce: Int) extends Animal(xPosition, yPosition, maxAge) {
   
    
    def receive = {
      
	     case Move => {
	       age += 1
	       changeLocation
	     }
	     
	     case Ping => self reply LynxPong(self)
	     
    }
    
  }
  
  class World() extends Actor {  
    
    /**
     * Values for the simulation
     * Uses values from the NetLogo Tutorial
     */
    private val worldSizeX = 100 //Not in NL Tutorial, but needed here
    private val worldSizeY = 100
    private val innitialHares = 100 //Recommended 0-1000
    private val initialLynx = 20 //Recommended 0-100
    private val hareBirthRate = 35 //Recommended 0-200
    private val maxHareAge = 6 //Recommended 0-20
    private val lynxEnergyToReproduce = 30 //Recommended 0-100
    private val engeryPerHareEaten = 10 //Recommended 0-30
    private val maxLynxAge = 20 //Recommended 0-50
    
    /**
     * Variables used by the simulation to manange the current state of the world.
     */
    private val rng = new Random();
    private var hares = new ArrayBuffer[ActorRef]()
    private var lynx = new ArrayBuffer[ActorRef]()
    private val activeHares = new ArrayBuffer[ActorRef]()
    private val activeLynx = new ArrayBuffer[ActorRef]()
    private val hareLocations = Array.ofDim[Set[ActorRef]](worldSizeX, worldSizeY)
    
    def receive = {
      case HarePong(actor) => {
        
      }
      case LynxPong(actor) => {
        
      }
    }
    
  }
  
}