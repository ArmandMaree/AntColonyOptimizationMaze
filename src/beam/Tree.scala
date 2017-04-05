import scala.collection.mutable.ListBuffer

package com.codehaven {
	object Tree {
		var root: Node = null
		var maze: Maze = null

		def printTreeToFile() = {
			ImageProcessor.outputMapAsImage(maze.maze, treeToMatrix)
		}

		def printSolution(finalNode: Node) : Int = {
			var pathLength = 0
			var matrix = Array.ofDim[Int](maze.numRows, maze.numCols)
			var tmpNode = finalNode

			(0 until maze.numRows).foreach(row => {
				(0 until maze.numCols).foreach(col => {
					matrix(row)(col) = 0
				})
			})

			while(tmpNode != null) {
				pathLength += 1
				matrix(tmpNode.pos._1)(tmpNode.pos._2) = 1
				tmpNode = tmpNode.parent
			}

			ImageProcessor.outputMapAsImage(maze.maze, matrix)
			return pathLength
		}

		def treeToMatrix() : Array[Array[Int]] = {
			var nodes = new ListBuffer[Node]()
			var matrix = Array.ofDim[Int](maze.numRows, maze.numCols)

			(0 until maze.numRows).foreach(row => {
				(0 until maze.numCols).foreach(col => {
					matrix(row)(col) = 0
				})
			})

			if(root != null){
				nodes += root
			}

			while(nodes.length != 0) {
				matrix(nodes(0).pos._1)(nodes(0).pos._2) = 1
				nodes = ListBuffer.concat(nodes, nodes(0).children)
				nodes.remove(0)
			}

			return matrix
		}
	}
}