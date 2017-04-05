import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.image.Raster
import java.io.File
import java.awt.Color

package com.codehaven {
	object ImageProcessor {
		var numRows = 0
		var numCols = 0
		var imageType = 0
		val BLACK = -16777216
		val WHITE = 16777215
		var RED = 16711680

		def getImageAsMatrix(mazeName: String) : Array[Array[Double]] = {
			printf("Reading maze: ./mazes/%s.bmp\n", mazeName)
			var inputImage = ImageIO.read(new File("mazes/" + mazeName + ".bmp"))

			var rasterImage = inputImage.getRaster()
			numRows = rasterImage.getHeight()
			printf("Height of image: %d\n", numRows)
			numCols = rasterImage.getWidth()
			printf("Width of image:  %d\n", numCols)
			imageType = inputImage.getType()
			printf("Type of image:   %d\n", imageType)

			var cellColor = Array.ofDim[Double](numRows, numCols)
			val defaultPheromone = (numRows + numCols) / 2
			println("Default Pheromone Level: " + defaultPheromone)

			// get average color of each cell in the input image
			(numRows - 1 to 0 by -1).foreach(row => {
				(0 until numCols).foreach(col => {
					cellColor(row)(col) = inputImage.getRGB(col, numRows - row - 1) match {
						case BLACK => -1
						case default => defaultPheromone
					}
				})
			})

			return cellColor
		}

		def outputMapAsImage(maze: Array[Array[Double]], path: Array[Array[Int]]) = {
			// RED =   1000
			var finalImg = new BufferedImage(numCols, numRows, 5)

			(numRows - 1 to 0 by -1).foreach(row => {
				(0 until numCols).foreach(col => {
					if(path(row)(col) != 1){
						finalImg.setRGB(col, numRows - row - 1, if (maze(row)(col) == -1) BLACK else WHITE)
						// printf("%s,", if (maze(row)(col) == -1) "0" else "1")
					}
					else {
						finalImg.setRGB(col, numRows - row - 1, RED)
						// RED += 1
						// RED %= 16777215
						// printf("%s,", "R")
					}
				})
				// println()
			})

			ImageIO.write(finalImg, "bmp", new File("output/path.bmp"));
		}

		def outputPheromoneAsImage(maze: Array[Array[Double]], end: (Int, Int)) = {
			var finalImg = new BufferedImage(numCols, numRows, 5)

			(numRows - 1 to 0 by -1).foreach(row => {
				(0 until numCols).foreach(col => {
					if(maze(row)(col) == -1){
						finalImg.setRGB(col, numRows - row - 1, BLACK)
						// printf("%s,", if (maze(row)(col) == -1) "0" else "1")
					}
					else {
						val p = Math.max(Math.floor(255 - 255 * (maze(row)(col) / maze(end._1)(end._2))).toInt, 0)
						// if(p < 0 || p > 255){
						// 	println("WRONG COLOR!!!! " + row + " " + col + " " + maze(row)(col) + " " + maze(end._1)(end._2) + " " + p)
						// }
						// println("VAL FOR " + (row, col) + ": " + p)
						val color = new Color(255, p, p)
						finalImg.setRGB(col, numRows - row - 1, color.getRGB)
						// RED += 1
						// RED %= 16777215
						// printf("%s,", "R")
					}
				})
				// println()
			})

			ImageIO.write(finalImg, "png", new File("output/heatmap.png"));
		}
	}
}
