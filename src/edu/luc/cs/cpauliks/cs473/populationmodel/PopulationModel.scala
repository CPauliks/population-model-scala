package edu.luc.cs.cpauliks.cs473.populationmodel

import akka.actor._
import akka.actor.Actor._
import scala.collection.mutable.HashSet
import scala.util.Random

object PopulationModel extends App {
  
  val world = actorOf[World].start()
  
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
  case class NewLynxLocation(oldX: Int, oldY: Int, newX: Int, newY: Int, hare: ActorRef) extends MessageFromAnimal
  case class LynxReproduced(xPos: Int, yPos: Int, newLynx: ActorRef) extends MessageFromAnimal
  case class LynxDied(xPos: Int, yPos: Int, lyxn: ActorRef) extends MessageFromAnimal
  case class LynxPong(lynx: ActorRef) extends MessageFromAnimal
  
  //Action
  case object Move extends MessageToAnimal
  case object Ping extends MessageToAnimal
  case object Eat extends MessageToAnimal 
  case object Reproduce extends MessageToAnimal
  case class CorrectLocation(xPos: Int, yPos: Int) extends MessageToAnimal
  
  abstract class Animal(xPosition: Int, yPosition: Int, maxAge: Int) extends Actor{
    val rng = new Random()
    var age = 0
    var xPos = xPosition
    var yPos = yPosition
    var oldX: Int = _
    var oldY: Int = _
    
    def changeLocation() = {
      oldX = xPos
      oldY = yPos
      val forwardX = rng.nextBoolean()
      val forwardY = rng.nextBoolean()
      xPos += (if(forwardX) rng.nextInt(2) else (rng.nextInt(2) * -1))
      yPos += (if(forwardY) rng.nextInt(2) else (rng.nextInt(2) * -1))
    }
    
    def checkIfStillAlive(): Boolean
    
  }
  
  class Hare(xPosition: Int, yPosition: Int, maxAge: Int) extends Animal(xPosition, yPosition, maxAge) {
    def receive = {
      
      case Ping => self reply HarePong(self)
      case Move => {
        age += 1
        if(checkIfStillAlive()) {
          changeLocation()
          self reply NewHareLocation(oldX, oldY, xPos, yPos, self)
        }
        else {
          self reply HareDied(xPos, yPos, self)
          self.stop()
        } 
      }
      case Reproduce => self reply HareCanReproduce(xPos, yPos)
      case CorrectLocation(x, y) => {
        xPos = x
        yPos = y
      }
      
    }
    
    
    def checkIfStillAlive() = {
      if (age <= maxAge) true else false
    }
    
  }
  
  class Lynx(xPosition: Int, yPosition: Int, maxAge: Int, startingEnergy: Int, energyPerHare: Int, energyToReproduce: Int) extends Animal(xPosition, yPosition, maxAge) {
   var energy = startingEnergy
   
    def receive = {
      case Ping => self reply LynxPong(self)
	  case Move => {
	    age += 1
	    energy -= 1
	    if(checkIfStillAlive()){
	      changeLocation()
	      self reply NewLynxLocation(oldX, oldY, xPos, yPos, self)
	    }  
	    else {
	      self reply LynxDied(xPos, yPos, self)
	      self.stop()
	    }
	  }
	  case Eat => energy += energyPerHare
	  case Reproduce => {
	    if(canReproduce()) {
	      energy = energy/2
	      val newLynx = actorOf(new Lynx(xPos, yPos, maxAge, energy, energyPerHare, energyToReproduce)).start()
	      self reply LynxReproduced(xPos, yPos, newLynx)
	    }
	  }
	  case CorrectLocation(x, y) => {
        xPos = x
        yPos = y
      }
    }
     
    def canReproduce() = {
      if (energy >= energyToReproduce) true else false
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
    
    /**
     * Variables used by the simulation to manage the current state of the world.
     */
    private val rng = new Random();
    private val hareSet = new HashSet[ActorRef]()
    private val lynxSet = new HashSet[ActorRef]()
    private val activeHares = new HashSet[ActorRef]()
    private val activeLynx = new HashSet[ActorRef]()
    private val hareLocations = Array.ofDim[HashSet[ActorRef]](worldSizeX, worldSizeY)
    private val lynxLocations = Array.ofDim[HashSet[ActorRef]](worldSizeX, worldSizeY)
    
    for(i <- 0 until worldSizeX) {
      for (j <- 0 until worldSizeY){
        hareLocations(i)(j) = new HashSet()
        lynxLocations(i)(j) = new HashSet()
      }
    }
    
    private var cycle = 0
    private var moving = false
    private var reproducing = false
    private var eating = true
    
    def receive = {
      
      case HarePong(actor) => {
        if(activeHares.contains(actor)) {
          activeHares -= actor
        }
        checkIfCompleteAndStartNextStage()
      }
      case HareDied(xPos, yPos, hare) => removeActorDueToDeath(xPos, yPos, hare, hareSet, activeHares, hareLocations)
      case NewHareLocation(oldX, oldY, xPos, yPos, hare) => moveActor(oldX, oldY, xPos, yPos, hare, hareSet, hareLocations)
      case HareCanReproduce(xPos, yPos) => tryCreateNewHareAtLocation(xPos, yPos)
        
      case LynxPong(actor) => {
        if(activeLynx.contains(actor)){
          activeLynx -= actor
        }
        checkIfCompleteAndStartNextStage()
      }
      case LynxDied(xPos, yPos, lynx) => removeActorDueToDeath(xPos, yPos, lynx, lynxSet, activeLynx, lynxLocations)
      case NewLynxLocation(oldX, oldY, xPos, yPos, lynx) => moveActor(oldX, oldY, xPos, yPos, lynx, lynxSet, lynxLocations)
      case LynxReproduced(xPos, yPos, lynx) => addActor(xPos, yPos, lynx, lynxSet, lynxLocations)
    }
    
    def checkIfCompleteAndStartNextStage() = {
      if(activeHares.isEmpty && activeLynx.isEmpty) {
        if(moving) {
          moving = false
          println ("Move phase complete!")
          startEat()
        }
        else if (reproducing) {
          reproducing = false
          println ("Reproduction phase complete!")
          cycleWrapUp()
          startMove()
        }
        else if (eating) {
          eating = false
          println ("Eat phase complete!")
          startReproduce()
        }
      }
    }
    
    def startEat() = {
      eating = true
      activeLynx ++= lynxSet
      tellLynxToEat()
      messageAll(hareSet, Ping)
      messageAll(lynxSet, Ping)
    }
    
    def startMove() = {
    	moving = true
    	activeHares ++= hareSet
    	activeLynx ++= lynxSet
    	messageAll(hareSet, Move)
    	messageAll(lynxSet, Move)
    	messageAll(hareSet, Ping)
    	messageAll(lynxSet, Ping)
    }
    
    def startReproduce() = {
      reproducing = true
      activeHares ++= hareSet
      activeLynx ++= lynxSet
      messageAll(hareSet, Reproduce)
      messageAll(lynxSet, Reproduce)
      messageAll(hareSet, Ping)
      messageAll(lynxSet, Ping)
    }
    
    def cycleWrapUp() = {
      println("Ending populations for cycle:  " + cycle)
      println("Hare population:  " + hareSet.size)
      println("Lynx population:  " + lynxSet.size)
      cycle += 1
      println("Press Enter to continue")
      val input = io.Source.stdin.getLine(0)
      if(hareSet.isEmpty || lynxSet.isEmpty) {
        println("The simulation has ended.")
        messageAll(hareSet, PoisonPill)
        messageAll(lynxSet, PoisonPill)
        self.stop()
      }
    }
    
    /**
     * Tries to create a Hare at the given location and add it to the simulation
     */
    def tryCreateNewHareAtLocation(xPos: Int, yPos: Int) = {
      if(rng.nextInt(maxHareBirthRate) <= hareBirthRate) {
        val trueX = correctPosition(xPos, worldSizeX)
        val trueY = correctPosition(yPos, worldSizeY)
        val newHare = actorOf(new Hare(trueX, trueY, maxHareAge)).start()
        addActor(trueX, trueY, newHare, hareSet, hareLocations)
      }
    }
    
    /**
     * Tells Lynx that are standing on Hares to gain energy as if they had eaten them.
     */
    def tellLynxToEat() = {
      var totalEaten = 0
      for(x <- 0 until worldSizeX) {
        for(y <- 0 until worldSizeY) {
          if(!lynxLocations(x)(y).isEmpty){
            val lynxThatEat  = lynxLocations(x)(y).take(hareLocations(x)(y).size)
            val haresThatDie = hareLocations(x)(y).take(lynxThatEat.size)
            messageAll(lynxThatEat, Eat)
            for(hare <- haresThatDie) {
              removeActor(x, y, hare, hareSet, activeHares, hareLocations)
            }
            totalEaten += lynxThatEat.size
            messageAll(haresThatDie, PoisonPill)
            
          }
            
        }
      }
      println("Hares eaten this cycle:  " + totalEaten)
    }
    
    /**
     * Adds a newly created actor to the simulation at a given location.
     */
    def addActor(xPos: Int, yPos: Int, actor: ActorRef, actorSet: HashSet[ActorRef], locationArray: Array[Array[HashSet[ActorRef]]]) = {
      actorSet += actor
      locationArray(xPos)(yPos) += actor
    }
    
    /**
     * Moves an ActorRef from one location to another as long as that ActorRef is still in use
     */
    def moveActor(oldX: Int, oldY: Int, xPos: Int, yPos: Int, actor: ActorRef, actorSet: HashSet[ActorRef], locationArray: Array[Array[HashSet[ActorRef]]]) = {
      if (actorSet contains actor) {
        val trueOldX = correctPosition(oldX, worldSizeX)
        val trueOldY = correctPosition(oldY, worldSizeY)
        val trueX = correctPosition(xPos, worldSizeX)
        val trueY = correctPosition(yPos, worldSizeY)
        if(trueX != xPos && trueY != yPos) actor ! CorrectLocation(trueX, trueY)
        locationArray(trueOldX)(trueOldY) -= actor
        locationArray(trueX)(trueY) += actor
      }
    }
    
    /**
     * Removes an Actor from all appropriate sets, as long as that ActorRef is still in use
     */
    def removeActor(xPos: Int, yPos: Int, actor: ActorRef, actorSet: HashSet[ActorRef], activeActors: HashSet[ActorRef], locationArray: Array[Array[HashSet[ActorRef]]]) = {
      if(actorSet contains actor) {
    	  val trueX = correctPosition(xPos, worldSizeX)
    	  val trueY = correctPosition(yPos, worldSizeY)
    	  actorSet -= actor
    	  activeActors -= actor
    	  locationArray(trueX)(trueY) -= actor
      }

    }
    
    def removeActorDueToDeath(xPos: Int, yPos: Int, actor: ActorRef, actorSet: HashSet[ActorRef], activeActors: HashSet[ActorRef], locationArray: Array[Array[HashSet[ActorRef]]]) = {
      if(actorSet contains actor) {
    	  val trueX = correctPosition(xPos, worldSizeX)
    	  val trueY = correctPosition(yPos, worldSizeY)
    	  actorSet -= actor
    	  activeActors -= actor
    	  locationArray(trueX)(trueY) -= actor
      }
      checkIfCompleteAndStartNextStage()
    }
    
    /**
     * Run after World.start() is called but before World starts accepting messages
     * Create all the Hares and Lynx and then start the simulation.
     */
    override def preStart() {
      createHares()
      createLynx()
      startMove()
    }
    

    /**
     * Corrects for ActorRefs moving outside the world by recalculating their coordinates as somewhere within the world
     * The world can be thought of as a 3d ellipsoid with xRadius worldSizeX/2 and yRadius worldSizeY/2
     * Values > worldSize are mapped as if they returned to 0
     * Values < 0 are mapped as if they returned to worldSize
     */
    def correctPosition(position: Int, worldSize: Int): Int = {
      if(position >= worldSize) correctPosition(position - worldSize, worldSize)
      else if (position < 0) correctPosition(position + worldSize, worldSize)
      else position
    }
    
    /**
     * Creates all the initial Hares for the simulation
     */
    def createHares() = {
      for (i <- 0 until initialHares) {
        val X = rng.nextInt(worldSizeX)
        val Y = rng.nextInt(worldSizeY)
        val newHare = actorOf(new Hare(X, Y, maxHareAge)).start()
        hareSet += newHare
        hareLocations(X)(Y) += newHare
      }
    }
    
    /**
     * Creates all the initial Lynx for the simulation
     */
    def createLynx() = {
      for (i <- 0 until initialLynx) {
        val X = rng.nextInt(worldSizeX)
        val Y = rng.nextInt(worldSizeY)
        val newLynx = actorOf(new Lynx(rng.nextInt(worldSizeX), rng.nextInt(worldSizeY), maxLynxAge, rng.nextInt(3 * energyPerHareEaten), energyPerHareEaten, lynxEnergyToReproduce)).start()
        lynxSet += newLynx
        lynxLocations(X)(Y) += newLynx
      }
    }
    
    /**
     * Send a message msg to all actors in a given Set
     */
    def messageAll(actors: HashSet[ActorRef], msg: Any) = {
     for (actor <- actors) {
       if(actor.isRunning) actor ! msg
     }
    }
    
  }
  
  
}