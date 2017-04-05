import scala.collection.mutable.ListBuffer

package com.codehaven {
	object Main {
		val maze = new Maze
		var nextMoves = new ListBuffer[Node]()
		var unexploredMoves = new ListBuffer[Node]()

		def main(args: Array[String]): Unit = {
			maze.init(args(0))
			Tree.maze = maze
			Tree.root = new Node(maze.start, null)
			maze.setCell(Tree.root.pos, 0)
			nextMoves += Tree.root

			// println("NEXT MOVES: " + nextMoves)
			beamSearch()
		}

		def beamSearch() = {
			var stop = false
			var numSteps = 0
			var finalNode: Node = null

			while(nextMoves.length > 0 && !stop){
				printf("\rSteps taken: %d", numSteps)
				finalNode = beamSearchStep()
				stop = finalNode != null
				// Tree.printTreeToFile()
				// Thread.sleep(100)
				numSteps += 1
				// println("====================================================")
				// stop = true
			}

			println()
			println("Length of path: " + Tree.printSolution(finalNode))
		}

		def beamSearchStep() : Node = {
			var parentChild = new ListBuffer[(Node, ListBuffer[Node])]()

			do {
				val children = findPossibleMoves(nextMoves(0))

				if(children.length > 0){
					val pc = (nextMoves(0), children)
					parentChild += pc
					// println("PC " + pc)
				}

				nextMoves.remove(0)
			}
			while (nextMoves.length > 0)

			if(parentChild.length == 0){
				if(unexploredMoves.length != 0) {
					nextMoves += unexploredMoves(unexploredMoves.length - 1)
					// println("PLACED " + unexploredMoves(unexploredMoves.length - 1) + " BACK")
					unexploredMoves(unexploredMoves.length - 1).parent.children += unexploredMoves(unexploredMoves.length - 1)
					unexploredMoves.remove(unexploredMoves.length - 1)
				}

				if(unexploredMoves.length != 0) {
					nextMoves += unexploredMoves(unexploredMoves.length - 1)
					// println("PLACED " + unexploredMoves(unexploredMoves.length - 1) + " BACK")
					unexploredMoves(unexploredMoves.length - 1).parent.children += unexploredMoves(unexploredMoves.length - 1)
					unexploredMoves.remove(unexploredMoves.length - 1)
				}

				if(nextMoves.length == 0){
					println("\nNO SOLUTION FOUND")
					return Tree.root
				}
			}
			else {
				var best: (Int, Int) = null
				var secondBest: (Int, Int) = null

				(0 until parentChild.length).foreach(i => {
					(0 until parentChild(i)._2.length).foreach(j => {

						if(best == null){
							best = (i, j)
						}
						else if(getEucleideanDist(parentChild(i)._2(j)) > getEucleideanDist(parentChild(best._1)._2(best._2))){
							if(secondBest != null){
								unexploredMoves += parentChild(secondBest._1)._2(secondBest._2)
								// println("SAVED: " + parentChild(secondBest._1)._2(secondBest._2) + " WITH PARENT " + parentChild(secondBest._1)._1)
							}

							secondBest = best
							best = (i, j)
						}
						else if (secondBest == null){
							secondBest = (i, j)
						}
						else if(getEucleideanDist(parentChild(i)._2(j)) > getEucleideanDist(parentChild(secondBest._1)._2(secondBest._2))){
							unexploredMoves += parentChild(secondBest._1)._2(secondBest._2)
							// println("SAVED: " + parentChild(secondBest._1)._2(secondBest._2) + " WITH PARENT " + parentChild(secondBest._1)._1)
							secondBest = (i, j)
						}
						else {
							unexploredMoves += parentChild(i)._2(j) 
							// println("SAVED: " + parentChild(i)._2(j) + " WITH PARENT " + parentChild(i)._1)
						}

						if(parentChild(i)._2(j).pos._1 == maze.end._1 && parentChild(i)._2(j).pos._2 == maze.end._2){
							parentChild(i)._2(j).parent.children += parentChild(i)._2(j)
							// println("\nSOLUTION FOUND!")
							return parentChild(i)._2(j)
						}
					})
				})

				if(best != null){
					// println("BEST: " + parentChild(best._1)._2(best._2))
					parentChild(best._1)._1.children += parentChild(best._1)._2(best._2)
					nextMoves += parentChild(best._1)._2(best._2)
					maze.setCell(parentChild(best._1)._2(best._2).pos, 0)

					if(secondBest != null){
						// println("SECONDBEST: " + parentChild(secondBest._1)._2(secondBest._2))
						parentChild(secondBest._1)._1.children += parentChild(secondBest._1)._2(secondBest._2)
						nextMoves += parentChild(secondBest._1)._2(secondBest._2)
						maze.setCell(parentChild(secondBest._1)._2(secondBest._2).pos, 0)
					}
				}
			}

			return null
		}

		def findPossibleMoves(currNode: Node) : ListBuffer[Node] = {
			val currPos = currNode.pos
			var possibleMoves = new ListBuffer[Node]()

			var returnedPostion = findNextPosAbove(currNode)
			if(returnedPostion != null){
				// println(returnedPostion+" IS UP WITH VALUE: " + r2)
				possibleMoves += returnedPostion
				maze.setCell(returnedPostion.pos, 2)
			}

			if(currPos._1 != maze.start._1 || currPos._2 != maze.start._2){ // not at start pos
				returnedPostion = findNextPosBelow(currNode)
				if(returnedPostion != null){
					// println(returnedPostion+" IS DOWN WITH VALUE: " + r2)
					possibleMoves += returnedPostion
					maze.setCell(returnedPostion.pos, 2)
				}
			}
		
			returnedPostion = findNextPosRight(currNode)
			if(returnedPostion != null){
				// println(returnedPostion+" IS RIGHT WITH VALUE: " + r2)
				possibleMoves += returnedPostion
				maze.setCell(returnedPostion.pos, 2)
			}
		
			returnedPostion = findNextPosLeft(currNode)
			if(returnedPostion != null){
				// println(returnedPostion+" IS LEFT WITH VALUE: " + r2)
				possibleMoves += returnedPostion
				maze.setCell(returnedPostion.pos, 2)
			}

			return possibleMoves
		}

		def findNextPosAbove(currNode: Node) : Node = {
			val currPos = currNode.pos
			var possibleMove : (Int, Int) = null
			
			if(maze.getCell(currPos._1 + 1, currPos._2) != -1){ // above is not a wall
				var newPos = (currPos._1, currPos._2)
				var up = 0.0
				var right = 0.0
				var left = 0.0

				do {
					newPos = (newPos._1 + 1, newPos._2)
					up = if(newPos._1  == maze.numRows - 1) -1 else maze.getCell(newPos._1 + 1, newPos._2)
					right = maze.getCell(newPos._1, newPos._2 + 1)
					left = maze.getCell(newPos._1, newPos._2 - 1)
				}
				while(up != -1 && right == -1 && left == -1)

				possibleMove = newPos
			}

			if (possibleMove != null && maze.getCell(possibleMove) == 1)
				return new Node(possibleMove, currNode)
			else
				return null
		}

		def findNextPosBelow(currNode: Node) : Node = {
			val currPos = currNode.pos
			var possibleMove : (Int, Int) = null

			if(maze.getCell(currPos._1 - 1, currPos._2) != -1){ // below is not a wall
				var newPos = (currPos._1, currPos._2)
				var down = 0.0
				var right = 0.0
				var left = 0.0

				do {
					newPos = (newPos._1 - 1, newPos._2)
					right = maze.getCell(newPos._1, newPos._2 + 1)
					down = if(newPos._1 == 0) -1 else maze.getCell(newPos._1 - 1, newPos._2)
					left = maze.getCell(newPos._1, newPos._2 - 1)
				}
				while(down != -1 && right == -1 && left == -1)

				possibleMove = newPos
			}

			if (possibleMove != null && maze.getCell(possibleMove) == 1)
				return new Node(possibleMove, currNode)
			else
				return null
		}

		def findNextPosRight(currNode: Node) : Node = {
			val currPos = currNode.pos
			var possibleMove : (Int, Int) = null
			
			if(maze.getCell(currPos._1, currPos._2 + 1) != -1){ // right is not a wall
				var newPos = (currPos._1, currPos._2)
				var up = 0.0
				var down = 0.0
				var right = 0.0

				do {
					newPos = (newPos._1, newPos._2 + 1)
					up = maze.getCell(newPos._1 + 1, newPos._2)
					right = maze.getCell(newPos._1, newPos._2 + 1)
					down = maze.getCell(newPos._1 - 1, newPos._2)
				}
				while(up == -1 && down == -1 && right != -1)

				possibleMove = newPos
			}

			if (possibleMove != null && maze.getCell(possibleMove) == 1)
				return new Node(possibleMove, currNode)
			else
				return null
		}

		def findNextPosLeft(currNode: Node) : Node = {
			val currPos = currNode.pos
			var possibleMove : (Int, Int) = null

			if(maze.getCell(currPos._1, currPos._2 - 1) != -1){ // left is not a wall
					var newPos = (currPos._1, currPos._2)
					var up = 0.0
					var down = 0.0
					var left = 0.0

					do {
						newPos = (newPos._1, newPos._2 - 1)

						up = maze.getCell(newPos._1 + 1, newPos._2)
						down = maze.getCell(newPos._1 - 1, newPos._2)
						left = maze.getCell(newPos._1, newPos._2 - 1)
					}
					while(up == -1 && down == -1 && left != -1)

				possibleMove = newPos
			}

			if (possibleMove != null && maze.getCell(possibleMove) == 1)
				return new Node(possibleMove, currNode)
			else
				return null
		}

		def getEucleideanDist(node : Node) : Double = {
			val yDiff = Math.pow(node.pos._1 - maze.end._1, 2)
			val xDiff = Math.pow(node.pos._2 - maze.end._2, 2)

			return Math.sqrt(yDiff + xDiff)
		}
	}
}
