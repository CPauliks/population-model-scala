package edu.luc.cs.cpauliks.cs473.populationmodel

import akka.actor._
import akka.actor.Actor._
import scala.collection.mutable.HashSet
import scala.util.Random

object PopulationModel extends App {
  
  /**
   * Messages passed between the World and the Animals
   */
  trait MessageFromAnimal
  trait MessageToAnimal
  
  case class NewHareLocation(oldX: Int, oldY: Int, newX: Int, newY: Int, hare: ActorRef) extends MessageFromAnimal
  case class HareCanReproduce(xPos: Int, yPos: Int) extends MessageFromAnimal
  case class HareDied(xPos: Int, yPos: Int, hare: ActorRef) extends MessageFromAnimal
  
  //TODO: Remove LynxLookingForHare as Lynx will not ask to eat, they will be told they got to eat
  case class LynxReproduced(newLynx: ActorRef) extends MessageFromAnimal
  case class LynxLookingForHare(xPos: Int, yPos: Int) extends MessageFromAnimal
  //TODO: Add xPos and yPox to LynxDied so that World can remove the Lynx from lynxLocations
  case class LynxDied(lynx: ActorRef) extends MessageFromAnimal
  
  case class HarePong(hare: ActorRef) extends MessageFromAnimal
  case class LynxPong(lynx: ActorRef) extends MessageFromAnimal
  
  case object Move extends MessageToAnimal
  case object Ping extends MessageToAnimal
  case object Eat extends MessageToAnimal 
  case object Reproduce extends MessageToAnimal
  
  abstract class Animal(xPosition: Int, yPosition: Int, maxAge: Int) extends Actor{
    val rng = new Random()
    var age = 0
    var xPos = xPosition
    var yPos = yPosition
    
    def changeLocation() = {
      val forwardX = rng.nextBoolean()
      val forwardY = rng.nextBoolean()
      xPos += (if(forwardX) rng.nextInt(2) else (rng.nextInt(2) * -1))
      yPos += (if(forwardY) rng.nextInt(2) else (rng.nextInt(2) * -1))
    }
    
    def checkIfStillAlive: Boolean
    
  }
  
  class Hare(xPosition: Int, yPosition: Int, maxAge: Int) extends Animal(xPosition, yPosition, maxAge) {
    def receive = {
      
      case Ping => self reply HarePong(self)
      case Move => {
        age += 1
        if(checkIfStillALive()) {
          val oldX = xPos
          val oldY = yPos
          changeLocation()
          self reply NewHareLocation(oldX, oldY, xPos, yPos, self)
        }
        else {
          self reply HareDied(xPos, yPos, self)
          self.stop()
        } 
      }
      case Reproduce =>
      
    }
    
    
    def checkIfStillALive() = {
      if (age <= maxAge) true else false
    }
    
  }
  
  class Lynx(xPosition: Int, yPosition: Int, maxAge: Int, startingEnergy: Int, energyPerHare: Int, energyToReproduce: Int) extends Animal(xPosition, yPosition, maxAge) {
   var energy = startingEnergy
   
   //TODO: Add chase case for Eat to simply add energyPerHare to energy, as the logic eating for should be moved to World
   
    def receive = {
      case Ping => self reply LynxPong(self)
	  case Move => {
	    age += 1
	    energy -= 1
	    if(checkIfStillAlive()){
	      changeLocation()
	    }  
	    else {
	      self reply LynxDied(self)
	      self.stop()
	    }
	  }
	  case Eat => tryToEat()
	  case Reproduce => 
    }
   
   //TODO: Delete this function, we don't need it anymore.
   def tryToEat() = {
     
   }
    
    def checkIfStillAlive() = {
      if(age <= maxAge && energy > 0) true else false
    }
    
  }
  
  class World() extends Actor {  
    
    /**
     * Values for the simulation
     * Uses values from the NetLogo Tutorial
     */
    private val worldSizeX = 100 //Not in NL Tutorial, but needed here
    private val worldSizeY = 100
    private val initialHares = 100 //Recommended 0-1000
    private val initialLynx = 20 //Recommended 0-100
    private val hareBirthRate = 35 //Recommended 0-200
    private val maxHareAge = 6 //Recommended 0-20
    private val lynxEnergyToReproduce = 30 //Recommended 0-100
    private val energyPerHareEaten = 10 //Recommended 0-30
    private val maxLynxAge = 20 //Recommended 0-50
    
    /**
     * Variables used by the simulation to manange the current state of the world.
     */
    private val rng = new Random();
    private val hares = new HashSet[ActorRef]()
    private val lynx = new HashSet[ActorRef]()
    private val activeHares = new HashSet[ActorRef]()
    private val activeLynx = new HashSet[ActorRef]()
    private val hareLocations = Array.ofDim[HashSet[ActorRef]](worldSizeX, worldSizeY)
    
    //TODO: add lynxLocations array same as hareLocations  
    
    private var moving = false;
    private var reproducing = false;
    private var eating = false;
    
    //TODO: Add cases for each messageFromAnimal
    
    def receive = {
      case HarePong(actor) => {
        if(activeHares.contains(actor)) {
          activeHares.remove(actor)
        }
        checkIfCompleteAndStartNextStage()
      }
      case LynxPong(actor) => {
        if(activeLynx.contains(actor)){
          activeLynx.remove(actor)
        }
        checkIfCompleteAndStartNextStage()
      }
      case LynxLookingForHare(x, y) => {
        self reply checkForHareAndEat(x, y)
      }
    }
    
    //TODO:  Add checkIfCompleteAndStartNextStage. Should make sure activeHares and activeLynx are empty and then check which boolean flag is set.  Flip to the next flag.  Repopulate ActiveHares and ActiveLynx. Send out the messages for the next tick. If we are starting the next year, print out the current size of hares and lynx
    
    override def prestart() {
      createHares()
      createLynx()
    }
    
    def createHares() = {
      for (i <- 0 until initialHares) {
        val X = rng.nextInt(worldSizeX)
        val Y = rng.nextInt(worldSizeY)
        val newHare = actorOf(new Hare(X, Y, maxHareAge)).start()
        hares += newHare
        hareLocations(X)(Y) += newHare
      }
    }
    
    def createLynx() = {
      //TODO: change this to the same style as createHares so that Lynx are added to lynxLocations
      for (i <- 0 until initialLynx) {
        lynx += actorOf(new Lynx(rng.nextInt(worldSizeX), rng.nextInt(worldSizeY), maxLynxAge, rng.nextInt(3 * energyPerHareEaten), energyPerHareEaten, lynxEnergyToReproduce)).start()
      }
    }
    
    //TODO: Change this method into looping through hareLocations and lynxLocations and check each. Lynx are more rare, so first check if lynx are at a location, and if so, see how many hares are there.  Kill as many hares as there are lynx, if possible, and message each lynx that got to eat.
    def checkForHareAndEat(xPos: Int, yPos: Int) =  {
      val trueX = if(xPos >= worldSizeX) xPos - worldSizeX else xPos
      val trueY = if(yPos >= worldSizeY) yPos - worldSizeY else yPos
      hareLocations(trueX)(trueY).headOption match {
        case Some(hare) => {
          hareLocations(trueX)(trueY) -= hare
          killHare(hare)
          true
        }
        case None => false
      }
    }
    
    def killHare(hareToKill: ActorRef) = {
      activeHares -= hareToKill
      hares -= hareToKill
      hareToKill ! PoisonPill
    }
    
  }
  
}