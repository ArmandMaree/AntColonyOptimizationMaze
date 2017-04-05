import scala.collection.mutable.ListBuffer
import util.control.Breaks._

package com.codehaven {
	class Ant(mazePar: Maze) {
		var maze = mazePar
		// var pos = maze.start
		// var prevPos = maze.start
		var path : ListBuffer[(Int,Int)] = null
		var memory: Array[Array[Int]] = null
		var saveList : ListBuffer[((Int,Int), (Int,Int))] = null

		def reset() = {
			path = new ListBuffer[(Int,Int)]()
			saveList = new ListBuffer[((Int,Int), (Int,Int))]()
			memory = Array.ofDim[Int](maze.numRows, maze.numCols)
			path += maze.start
			memory(maze.start._1)(maze.start._2) = 1

			memory.foreach(row => {
				(0 until row.length).foreach(i => {
					row(i) = 0
				})
			})
		}

		def printPath(once: Boolean = true): Thread = {
			if(once){
				Ant.printPath(maze, memory)
				return null
			}
			else {
				// var runnable = new Runnable {
				// 	def run() {
				// 		while(true){
				// 			// println("print")
				// 			Ant.printPath(maze, memory)
				// 			Thread.sleep(500)
				// 		}
				// 	}
				// }
				// val printThread = new Thread(runnable)
				// printThread.start()
				// return printThread
			}
			return null
		}

		def getPos() = {
			path(path.length - 1)
		}

		def getPrevPos() = {
			if(path.length > 1){
				path(path.length - 2)
			}
			else{
				(-1,-1)
			}
		}

		def addToPath(targetCell: (Int, Int)) = {
			path += targetCell
		}
	}

	object Ant {
		val randomGenerator = scala.util.Random
		val ABOVE = 0
		val LEFT = 1
		val BELOW = 2
		val RIGHT = 3

		def moveAnt(ant: Ant) : Double = {
			// if(Thread.currentThread().getId == 10){
			// 	println("PATH: "  + ant.path)	
			// }
			val (possibleMoves, possibleMovesValues) = findPossibleMoves(ant)

			val randomNumber = randomGenerator.nextDouble()
			var targetCell : (Int, Int) = null
			var index = 0
			var cumulativePercentage = 0.0
			var sum = 0.0

			possibleMovesValues.foreach(moveValue => {
				sum += moveValue
			})

			do { // select a target cell
				cumulativePercentage += possibleMovesValues(index) / sum

				if(targetCell == null && randomNumber <= cumulativePercentage){
					targetCell = possibleMoves(index)
				}
				else {
					val tmp = (ant.getPos(), possibleMoves(index))
					ant.memory(possibleMoves(index)._1)(possibleMoves(index)._2) = 2
					ant.saveList += tmp
				}

				index += 1
			}
			while(index < possibleMoves.length)

			ant.addToPath(targetCell)
			ant.memory(targetCell._1)(targetCell._2) = 1
			// detectAndRemoveLoops(ant)

			if(ant.getPos()._1 == ant.maze.end._1 && ant.getPos()._2 == ant.maze.end._2){ // is at exit
				val pheromoneDelta = ant.path.length //1.0 / 
				ant.path.foreach(pos => {
					ant.maze.addFerromone(pos._1, pos._2, pheromoneDelta)
				})

				return pheromoneDelta
			}
			else{
				return -1
			}
		}

		// def detectAndRemoveLoops(ant: Ant) = {
		// 	// println("THREAD: " + Thread.currentThread().getId())
		// 	var foundLoop = false
		// 	var index = 0
		// 	// println(ant.path)

		// 	while(index < ant.path.length - 1 && !foundLoop) {
		// 		val posInPath = ant.path(index)
		// 		// println("COMPARING "+ant.getPos() + " AND " + posInPath + " AT INDEX " + index + "   LENGTH: " + ant.path.length)
		// 		if(ant.getPos()._1 == posInPath._1 && ant.getPos()._2 == posInPath._2){ // last pos in ant.path == pos at index of ant.path
		// 			foundLoop = true
		// 			// println("FOUND LOOP ADDED: " + ant.getPos() )
		// 			ant.addToAvoidList(ant.getPos())
		// 			ant.path.remove(ant.path.length - 1)
		// 			// println("Reverted to: " + ant.getPos()  + "  with prev: " + ant.getPrevPos())
		// 		}	
		// 		else {
		// 			index += 1
		// 		}
		// 	}
		// }

		def printPath(maze: Maze, memory: Array[Array[Int]]) = { // TODO: CHECK THIS->SECOND PARAM
			ImageProcessor.outputMapAsImage(maze.maze, memory)
		}

		// def generatePathMatrix(path: ListBuffer[(Int,Int)], numRows: Int, numCols: Int) : Array[Array[Int]] = {
		// 	var pathMatrix = Array.ofDim[Int](numCols, numRows)

		// 	(numRows - 1 to 0 by -1).foreach(row => {
		// 		(0 until numCols).foreach(col => {
		// 			pathMatrix(row)(col) = 0
		// 		})
		// 	})

		// 	path.foreach(pos => {
		// 		pathMatrix(pos._1)(pos._2) = 1
		// 	})

		// 	return pathMatrix
		// }

		def findPossibleMoves(ant: Ant) : (ListBuffer[(Int,Int)], ListBuffer[Double]) = {
			var possibleMoves = new ListBuffer[(Int,Int)]()
			var possibleMovesValues = new ListBuffer[Double]()
			var functionIndex = 0 // which function to call
			var up = false

			var returnedPostion = findNextPosAbove(ant)
			if(returnedPostion != null){
				// println(returnedPostion+" IS UP WITH VALUE: " + r2)
				possibleMoves += returnedPostion
				possibleMovesValues += ant.maze.getCell(returnedPostion._1, returnedPostion._2)// * 1.5
			}

			if(ant.getPos()._1 != ant.maze.start._1 || ant.getPos()._2 != ant.maze.start._2){ // ant not at start pos
				returnedPostion = findNextPosBelow(ant)
				if(returnedPostion != null){
					// println(returnedPostion+" IS DOWN WITH VALUE: " + r2)
					possibleMoves += returnedPostion
					possibleMovesValues += ant.maze.getCell(returnedPostion._1, returnedPostion._2)* 1.5
				}
			}
		
			returnedPostion = findNextPosRight(ant)
			if(returnedPostion != null){
				// println(returnedPostion+" IS RIGHT WITH VALUE: " + r2)
				possibleMoves += returnedPostion
				if(returnedPostion._2 > ant.maze.end._2){
					possibleMovesValues += ant.maze.getCell(returnedPostion._1, returnedPostion._2) * 1.5
				}
				else{
					possibleMovesValues += ant.maze.getCell(returnedPostion._1, returnedPostion._2)
				}
			}
		
			returnedPostion = findNextPosLeft(ant)
			if(returnedPostion != null){
				// println(returnedPostion+" IS LEFT WITH VALUE: " + r2)
				possibleMoves += returnedPostion
				if(returnedPostion._2 < ant.maze.end._2){
					possibleMovesValues += ant.maze.getCell(returnedPostion._1, returnedPostion._2) * 1.5
				}
				else{
					possibleMovesValues += ant.maze.getCell(returnedPostion._1, returnedPostion._2)
				}
			}

			// if(possibleMoves.length > 0){ // dont check avoidList if no possibleMoves exist
			// 	var i = 0
			// 	while(i < possibleMoves.length){
			// 		if(inAvoidList(ant, possibleMoves(i))) {
			// 			possibleMoves.remove(i)
			// 			possibleMovesValues.remove(i)
			// 		}
			// 		else {
			// 			i += 1
			// 		}
			// 	}
			// }

			if(possibleMoves.length == 0){
				// println(ant.getPrevPos()+" IS PREV")
				// ant.addToAvoidList(ant.getPos())
				var nextMoveParent = ant.saveList(ant.saveList.length - 1)._1
				// println("PARENT: " + nextMoveParent)
				var nextMove = ant.saveList(ant.saveList.length - 1)._2
				// println("NEXTMOVE: " + nextMove)
				ant.saveList.remove(ant.saveList.length - 1)

				while(ant.path.length != 0 && !(ant.getPos()._1 == nextMoveParent._1 && ant.getPos()._2 == nextMoveParent._2)) {
					ant.memory(ant.getPos()._1)(ant.getPos()._2) = -1
					// println("REMOVE: " + ant.getPos())
					ant.path.remove(ant.path.length - 1)
					// println("LAST IN PATH: " + ant.getPos())
				}

				possibleMoves += nextMove
				possibleMovesValues += ant.maze.getCell(nextMove._1, nextMove._2)
			}


			breakable {
				(0 until possibleMoves.length).foreach(i => {
					if(possibleMoves(i)._1 == ant.maze.end._1 && possibleMoves(i)._2 == ant.maze.end._2){
						var tmp = possibleMoves(i)
						possibleMoves = new ListBuffer[(Int,Int)]()
						possibleMoves += tmp
						var tmp2 = possibleMovesValues(i)
						possibleMovesValues = new ListBuffer[Double]()
						possibleMovesValues += tmp2
						break
					}
				})
			}

			return (possibleMoves, possibleMovesValues)
		}

		def findNextPosAbove(ant: Ant) : (Int,Int) = {
			var possibleMove : (Int, Int) = null
			var value = 0.0
			if(!(ant.getPos()._1 < ant.getPrevPos()._1 && ant.getPos()._2 == ant.getPrevPos()._2) && ant.maze.getCell(ant.getPos()._1 + 1, ant.getPos()._2) != -1){ // not coming from below and above is not a wall
				var newPos = (ant.getPos()._1, ant.getPos()._2)
				var up = 0.0
				var right = 0.0
				var left = 0.0

				do {
					newPos = (newPos._1 + 1, newPos._2)
					up = if(newPos._1 == ant.maze.numRows - 1) -1 else ant.maze.getCell(newPos._1 + 1, newPos._2)
					right = ant.maze.getCell(newPos._1, newPos._2 + 1)
					left = ant.maze.getCell(newPos._1, newPos._2 - 1)
				}
				while(up != -1 && right == -1 && left == -1)

				possibleMove = newPos
				value = ant.maze.getCell(newPos._1, newPos._2)
			}

			if(possibleMove != null && ant.memory(possibleMove._1)(possibleMove._2) != 0 && ant.memory(possibleMove._1)(possibleMove._2) != 2){
				return null
			}
			else {
				return possibleMove
			}
		}

		def findNextPosBelow(ant: Ant) : (Int,Int) = {
			var possibleMove : (Int, Int) = null
			var possibleMovePrev : (Int, Int) = null
			var value = 0.0
			if(!(ant.getPos()._1 > ant.getPrevPos()._1 && ant.getPos()._2 == ant.getPrevPos()._2) && ant.maze.getCell(ant.getPos()._1 - 1, ant.getPos()._2) != -1){ // not coming from above and below is not a wall
				var newPos = (ant.getPos()._1, ant.getPos()._2)
				var down = 0.0
				var right = 0.0
				var left = 0.0

				do {
					newPos = (newPos._1 - 1, newPos._2)
					right = ant.maze.getCell(newPos._1, newPos._2 + 1)
					down = if(newPos._1 == 0) -1 else ant.maze.getCell(newPos._1 - 1, newPos._2)
					left = ant.maze.getCell(newPos._1, newPos._2 - 1)
				}
				while(down != -1 && right == -1 && left == -1)

				possibleMove = newPos
				value = ant.maze.getCell(newPos._1, newPos._2)
			}

			if(possibleMove != null && ant.memory(possibleMove._1)(possibleMove._2) != 0 && ant.memory(possibleMove._1)(possibleMove._2) != 2){
				return null
			}
			else {
				return possibleMove
			}
		}

		def findNextPosRight(ant: Ant) : (Int,Int) = {
			var possibleMove : (Int, Int) = null
			var value = 0.0
			if(!(ant.getPos()._1 == ant.getPrevPos()._1 && ant.getPos()._2 < ant.getPrevPos()._2) && ant.maze.getCell(ant.getPos()._1, ant.getPos()._2 + 1) != -1){ // not coming from right and right is not a wall
				var newPos = (ant.getPos()._1, ant.getPos()._2)
				var up = 0.0
				var down = 0.0
				var right = 0.0

				do {
					newPos = (newPos._1, newPos._2 + 1)

					up = ant.maze.getCell(newPos._1 + 1, newPos._2)
					right = ant.maze.getCell(newPos._1, newPos._2 + 1)
					down = ant.maze.getCell(newPos._1 - 1, newPos._2)
				}
				while(up == -1 && down == -1 && right != -1)

				possibleMove = newPos
				value = ant.maze.getCell(newPos._1, newPos._2)
			}

			if(possibleMove != null && ant.memory(possibleMove._1)(possibleMove._2) != 0 && ant.memory(possibleMove._1)(possibleMove._2) != 2){
				return null
			}
			else {
				return possibleMove
			}
		}

		def findNextPosLeft(ant: Ant) : (Int,Int) = {
			var possibleMove : (Int, Int) = null
			var possibleMovePrev : (Int, Int) = null
			var value = 0.0
			if(!(ant.getPos()._1 == ant.getPrevPos()._1 && ant.getPos()._2 > ant.getPrevPos()._2) && ant.maze.getCell(ant.getPos()._1, ant.getPos()._2 - 1) != -1){ // not coming from left and left is not a wall
					var newPos = (ant.getPos()._1, ant.getPos()._2)
					var up = 0.0
					var down = 0.0
					var left = 0.0

					do {
						newPos = (newPos._1, newPos._2 - 1)

						up = ant.maze.getCell(newPos._1 + 1, newPos._2)
						down = ant.maze.getCell(newPos._1 - 1, newPos._2)
						left = ant.maze.getCell(newPos._1, newPos._2 - 1)
					}
					while(up == -1 && down == -1 && left != -1)

				possibleMove = newPos
				value = ant.maze.getCell(newPos._1, newPos._2)
			}

			if(possibleMove != null && ant.memory(possibleMove._1)(possibleMove._2) != 0 && ant.memory(possibleMove._1)(possibleMove._2) != 2){
				return null
			}
			else {
				return possibleMove
			}
		}

		// def inAvoidList(ant: Ant, pos: (Int, Int)) : Boolean = {
		// 	return ant.avoidList(pos._1)(pos._2)
		// }
	}
}
