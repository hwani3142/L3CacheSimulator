package cacheSimulator

import scala.collection.mutable.Queue

case class StreamBuffer(size:Int) {
  var buf = Queue[Long](-1)
  
  def checkContains(contentTag:Long):Boolean = {
    if (buf.contains(contentTag)) {
      true
    }
    else {
      false
    }
  }
  def findEmptyBuffer() = {
    if (buf(0) == -1) true
    else false
  }
  // all miss case
  def preFetchAll(contentTag:Long) = {
    // dequeue all element
    for {
      i <- 0 until buf.size
    } {
    	buf.dequeue
    }
    // enqueue sequence element
    for {
      i <- 0 until size
    } {
      buf.enqueue((contentTag + i))
    }
  }
  // hit case
  def preFetch(contentTag:Long) = {
//		buf(index) = (buf.max+1)
    val index = buf.indexOf(contentTag)
    for {
      i <- 0 to index
    } {
      buf.dequeue
      buf.enqueue((buf.last + 1))
    }
  }
}
