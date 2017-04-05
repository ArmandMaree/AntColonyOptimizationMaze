compiler = scalac
runtime = scala
make:
	$(compiler) ImageProcessor.scala Maze.scala Node.scala Tree.scala Main.scala

run:
	$(runtime) -J-Xms1000m -J-Xmx4000m com.codehaven.Main Large1

all:
	make
	make run
	make clean

clean:
	rm -rf ./com