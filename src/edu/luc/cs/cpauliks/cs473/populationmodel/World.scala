package edu.luc.cs.cpauliks.cs473.populationmodel

import akka.actor._
import akka.actor.Actor._
import scala.collection.mutable.HashSet
import scala.util.Random
import edu.luc.cs.cpauliks.cs473.populationmodel.SimulationMesages._

class World(worldSizeX: Int, worldSizeY: Int, initialHares: Int, initialLynx: Int, hareBirthRate: Int, maxHareBirthRate: Int, maxHareAge: Int, lynxEnergyToReproduce: Int, energyPerHareEaten: Int, maxLynxAge: Int) extends Actor {  

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

	//Initialize the location arrays.
	for(i <- 0 until worldSizeX) {
		for (j <- 0 until worldSizeY){
			hareLocations(i)(j) = new HashSet()
			lynxLocations(i)(j) = new HashSet()
		}
	}

	private var cycle = 0
	private var moving = false
	private var reproducing = false
	private var eating = false
	private var aging = false
	

	/**
	 * Receive method for World
	 */
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
	case Start => startMove()
	}

	/**
	 * Determine if a phase is complete
	 * If so, move on to the next phase of the cycle.
	 */
	def checkIfCompleteAndStartNextStage() = {
		if(activeHares.isEmpty && activeLynx.isEmpty) {
			if(moving) {
				moving = false
				startEat()
			}
			else if (reproducing) {
				reproducing = false
				startAge()
			}
			else if (eating) {
				eating = false
				startReproduce()
			}
			else if (aging) {
			  aging = false
			  cycleWrapUp() 
			  startMove()
			}
		}
	}

	/**
	 * Steps for the eating phase of a cycle
	 * Tell certain Lynx to Eat, but we Ping everyone just to make sure they are still responding
	 */
	def startEat() = {
		eating = true
		activeLynx ++= lynxSet
		tellLynxToEat()
		messageAll(hareSet, Ping)
		messageAll(lynxSet, Ping)
	}

	/**
	 * Steps for the movement phase of a cycle
	 * Tell everyone to move and then wait for them to finish
	 */
	def startMove() = {
		moving = true
		activeHares ++= hareSet
		activeLynx ++= lynxSet
		messageAll(hareSet, Move)
		messageAll(lynxSet, Move)
		messageAll(hareSet, Ping)
		messageAll(lynxSet, Ping)
	}

	/**
	 * Steps for the reproduction phase of a cycle
	 * Tell everyone to reproduce and then wait for them to finish.
	 */
	def startReproduce() = {
		reproducing = true
		activeHares ++= hareSet
		activeLynx ++= lynxSet
		messageAll(hareSet, Reproduce)
		messageAll(lynxSet, Reproduce)
		messageAll(hareSet, Ping)
		messageAll(lynxSet, Ping)
	}
	
	/**
	 * Steps for the aging phase of a cycle
	 * Tell everyone to Age and then wait for them to finish.
	 */
	def startAge() = {
	  aging = true
	  activeHares ++= hareSet
	  activeHares ++= hareSet
	  activeLynx ++= lynxSet
	  messageAll(hareSet, Age)
	  messageAll(lynxSet, Age)
	  messageAll(hareSet, Ping)
	  messageAll(lynxSet, Ping)
	}

	/**
	 * Print out the information from the last cycle.
	 */
	def cycleWrapUp() = {
		println("Ending populations for cycle  " + cycle)
		println("Hare population:  " + hareSet.size)
		println("Lynx population:  " + lynxSet.size)
		cycle += 1
		println("Press Enter to continue or type exit to end the simulation early")
		val input = io.Source.stdin.getLine(0).trim()
		var exit = false
		if(input != null) {
		  exit = if (input.equalsIgnoreCase("exit")) true else false
		}
		if(hareSet.isEmpty || lynxSet.isEmpty || exit ) {
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
	  
		for(x <- 0 until worldSizeX) {
			for(y <- 0 until worldSizeY) {
				if(!lynxLocations(x)(y).isEmpty) {
					//Get as many Lynx as there are Hares at the location.
					val lynxThatEat  = lynxLocations(x)(y).take(hareLocations(x)(y).size)
					//Get as many Hares to kill as we have Lynx to feed
					val haresThatDie = hareLocations(x)(y).take(lynxThatEat.size)
					//Tell Lynx to gain energy
					messageAll(lynxThatEat, Eat)
					//Kill those Hares we ate
					for(hare <- haresThatDie) {
						removeActor(x, y, hare, hareSet, activeHares, hareLocations)
					}
					messageAll(haresThatDie, PoisonPill)
				}

			}
		}
	}

	/**
	 * Adds a newly created actor to the simulation at a given location.
	 */
	def addActor(xPos: Int, yPos: Int, actor: ActorRef, actorSet: HashSet[ActorRef], locationArray: Array[Array[HashSet[ActorRef]]]) = {
		
	  val trueX = correctPosition(xPos, worldSizeX)
	  val trueY = correctPosition(yPos, worldSizeY)
	  actorSet += actor
	  locationArray(trueX)(trueY) += actor
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

	/**
	 * Removes an Actor from all appropriate sets, as long as that ActorRef is still in use
	 */
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
	}
	
	/**
	 * End the program once the world has been stopped.
	 */
	override def postStop() {
	  exit(0)
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