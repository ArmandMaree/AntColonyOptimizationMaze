package com.codehaven {
	object Main {
		var numThreads = Runtime.getRuntime().availableProcessors() - 1
		var colonySize = numThreads
		// numThreads = 1
		// colonySize = 1
		var colony = new Array[Ant](colonySize)
		var antsInThread = colonySize / numThreads
		val maze = new Maze
		var mazeThread : Thread = null

		var bestPathIteration = 0
		var foundPath = false
		var bestPath = Double.MinValue
		var iterationCounter = new Array[Int](numThreads)
		var threads = new Array[Thread](numThreads)
		var runnable = new Array[Runnable](numThreads)
		var recalculateAnts = false

		def checkAndSetBest(newPath: Double, antIndex: Int, threadId: Int) = {this.synchronized{
			if(newPath > bestPath){
				println()
				colony(antIndex).printPath(true)
				bestPath = newPath
				bestPathIteration = iterationCounter(threadId)
				(0 until numThreads).foreach(i => {
					iterationCounter(i) = 0

					if(!threads(i).isAlive()){
						threads(i) = new Thread(runnable(i))
						threads(i).start()
					}
				})

				if(!foundPath){
					var newColony = new Array[Ant](numThreads * 4)

					(0 until colonySize).foreach(i => {
						newColony(i) = colony(i)
					})

					colonySize = numThreads * 4

					(colony.length until colonySize).foreach(i => {
						newColony(i) = new Ant(maze)
						newColony(i).reset
					})

					println("\rIncreased colony size from " + colony.length + " to " + newColony.length + "               ")
					colony = newColony
					antsInThread = colonySize / numThreads
					recalculateAnts = true
					foundPath = true
				}
			}
		}}

		def main(args: Array[String]): Unit = {
			maze.init(args(0))

			(0 until colonySize).foreach(i => {
				colony(i) = new Ant(maze)
				colony(i).reset
			})

			(0 until numThreads).foreach(i => {
				iterationCounter(i) = 0
			})

			var currId = 0

			def getId() : Int = {this.synchronized {
				val id = currId
				currId += 1
				return id
			}}

			val maintenanceThread = new Thread() {
				override def run() {
					while(true){
						(0 until 10).foreach(i => {
							maze.reducePheromone()
							Thread.sleep(10)
						})

						// colony(0).printPath() // debug; let ant 0 periodically print its path
					}
				}
			}

			maintenanceThread.start()

			(0 until numThreads).foreach(i => {
				runnable(i) = new Runnable {
					val myId = getId()
					var recalculatedAnts =  false
					var maxIterations = 5000
					def run() {
						var minAntIndex = myId * antsInThread
						var maxAntIndex = (myId + 1) * antsInThread
						// println("THREAD: " + Thread.currentThread().getId() + " HAS ID " + myId)
						println("THREAD: " + Thread.currentThread().getId() + " MIN: " + minAntIndex + " MAX: " + maxAntIndex)

						do {
							if(recalculateAnts && !recalculatedAnts){
								minAntIndex = myId * antsInThread
								maxAntIndex = (myId + 1) * antsInThread
								println("THREAD: " + Thread.currentThread().getId() + " MIN: " + minAntIndex + " MAX: " + maxAntIndex)
								recalculatedAnts = true
							}

							(minAntIndex until maxAntIndex).foreach(i => {
								// println("THREAD: " + myId + " ANT " + i)
								var newPath = Ant.moveAnt(colony(i))

								if(newPath != -1){
									// println("\rAnt #" + i + " found a path of length " + (newPath) + "                                                       ")
									checkAndSetBest(newPath, i, myId)
									colony(i).reset()
								}
							})

							iterationCounter(myId) += 1
							maxIterations = Math.max(bestPathIteration * 2, 10000)

							if(myId== 0){
								printf("\rTHREAD: " + myId + "   Found path: %s. Current iteration: %d (%d). END: %f            ", foundPath, iterationCounter(myId), maxIterations, maze.getCell(maze.end._1, maze.end._2))
								// maze.reducePheromone()
							}
							// Thread.sleep(250)
							// println("\n========================================")
						}
						while (iterationCounter(myId) < maxIterations || !foundPath)
					}
				}
				threads(i) = new Thread(runnable(i))
				threads(i).start()
			})

			threads.foreach(thread => {
				thread.join()
			})

			maintenanceThread.stop
			maze.printHeatMap()
			Thread.sleep(500)

			printf("\n\nFound path: %s at iteration: %d with length %d.         \n", foundPath, bestPathIteration, Math.round(bestPath))
			// maze.printTrueMaze()
		}
	}
}

// REPORT: MATRIX MEMORY
// REPORT: LITTLE START ANTS