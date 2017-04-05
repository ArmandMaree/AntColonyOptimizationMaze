package com.codehaven {
	class Maze {
		var mazeName = ""
		var maze = Array.ofDim[Int](0,0)
		var start = (0,0)
		var end = (0,0)
		var numRows = 0
		var numCols = 0

		def init(mazeNamePar: String) = {
			mazeName = mazeNamePar
			maze = ImageProcessor.getImageAsMatrix(mazeName)
			start = getStartPos(maze)
			end = getEndPos(maze)
			numRows = maze.length
			numCols = maze(0).length
			printf("Start position:  (%d,%d)\n", start._1, start._2)
			printf("End position:    (%d,%d)\n", end._1, end._2)
		}

		def setCell(pos: (Int, Int), value: Int) = {
			maze(pos._1)(pos._2) = value
		}

		def getCell(x: Int, y: Int) : Int = {
			return maze(x)(y)
		}

		def getCell(pos: (Int, Int)) : Int = {
			return maze(pos._1)(pos._2)
		}

		def getStartPos(maze: Array[Array[Int]]) : (Int, Int) = {
			maze(0).zipWithIndex foreach { case(value, index) => {
				if(value != -1) return (0, index)
			}}
			
			return (0,0)
		}

		def getEndPos(maze: Array[Array[Int]]) : (Int, Int) = {
			maze(maze.length - 1).zipWithIndex foreach { case(value, index) => {
				if(value != -1) return (maze.length - 1, index)
			}}

			return (0,0)
		}
	}
}
