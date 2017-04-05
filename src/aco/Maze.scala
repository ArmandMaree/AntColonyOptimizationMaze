package com.codehaven {
	class Maze {
		var mazeName = ""
		var maze = Array.ofDim[Double](0,0)
		var start = (0,0)
		var end = (0,0)
		var numRows = 0
		var numCols = 0
		var MAX_FERROMONE = 1.0
		var MIN_FERROMONE = (numRows + numCols) / 2
		def init(mazeNamePar: String) = {
			mazeName = mazeNamePar
			maze = ImageProcessor.getImageAsMatrix(mazeName)
			start = getStartPos(maze)
			end = getEndPos(maze)
			numRows = maze.length
			numCols = maze(0).length
			MIN_FERROMONE = (numRows + numCols) / 2
			MAX_FERROMONE = numRows * numCols
			printf("Start position:  (%d,%d)\n", start._1, start._2)
			printf("End position:    (%d,%d)\n", end._1, end._2)
		}

		def reducePheromone() = {
			(numRows - 1 to 0 by -1).foreach(row => {
				(0 until numCols).foreach(col => {
					if(maze(row)(col) != -1){
						setPheromone(row, col, 0.9999, 2)
					}
				})
			})
			// println("DECREASED! "+getCell(end._1, end._2))
		}

		def printHeatMap() = {
			ImageProcessor.outputPheromoneAsImage(maze, end)
		}

		def getCell(x: Int, y: Int) : Double = {
			maze(x)(y)
		}

		def getStartPos(maze: Array[Array[Double]]) : (Int, Int) = {
			maze(0).zipWithIndex foreach { case(value, index) => {
				if(value != -1) return (0, index)
			}}
			
			return (0,0)
		}

		def getEndPos(maze: Array[Array[Double]]) : (Int, Int) = {
			maze(maze.length - 1).zipWithIndex foreach { case(value, index) => {
				if(value != -1) return (maze.length - 1, index)
			}}

			return (0,0)
		}

		def addFerromone(row: Int, col: Int, ferromoneDelta: Double) = {
			setPheromone(row, col, ferromoneDelta, 1)

		}

		def setPheromone(row: Int, col: Int, value: Double, op: Int) = {this.synchronized{
				op match {
					case 0 => maze(row)(col) = value
					case 1 => maze(row)(col) += value
					case 2 => maze(row)(col) *= value
				}
				if(maze(row)(col) > MAX_FERROMONE){
					maze(row)(col) = MAX_FERROMONE
				}
				else if(maze(row)(col) < MIN_FERROMONE){
					maze(row)(col) = MIN_FERROMONE
				}
			}
		}

		def printMaze() = {
			(numRows - 1 to 0 by -1).foreach(row => {
				(0 until numCols).foreach(col => {
					if(col == numCols - 1){
						printf("%d", if (maze(row)(col) == -1) 0 else 1)
					}
					else {
						printf("%d,", if (maze(row)(col) == -1) 0 else 1)
					}
				})
				println()
			})
		}

		def printTrueMaze() = {
			(numRows - 1 to 0 by -1).foreach(row => {
				(0 until numCols).foreach(col => {
					if(col == numCols - 1){
						printf("%.3f", maze(row)(col))
					}
					else {
						printf("%.3f,", maze(row)(col))
					}
				})
				println()
			})
		}
	}
}
