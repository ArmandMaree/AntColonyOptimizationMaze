import scala.collection.mutable.ListBuffer

package com.codehaven {
	class Node(pos_ : (Int, Int), parent_ : Node) {
		val pos = pos_
		val parent = parent_
		var children = new ListBuffer[Node]() 

		override def toString(): String =  {
			return "Node" + pos
		} 
	}
}