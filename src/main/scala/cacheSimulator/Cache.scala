package cacheSimulator

import cacheSimulator.ConstantObject._
import scala.math.pow
import scala.collection.mutable.Queue
import scala.util.Random

case class Cache 
  (identifier:String, 
	l:Int, k:Int, size:Int){
  
  // Counting value
  var totalCount = 0
  var hitCount = 0
  var missCount = 0
  
  var instrReadMissCount = 0
  var dataReadMissCount = 0
  var dataWriteMissCount = 0
  
  // sub cache
  var subCache:Cache = _
  val level = this.identifier.split(" ")(0)
 
  val (tagBit, indexBit, offsetBit, n) = {
    val blockSize = l * WORD_SIZE
    val size = this.size * KB
    val numOfBlocks = size / blockSize
    val numOfSets = numOfBlocks / this.k
    val offset = log2(l)
    val index = log2(numOfSets)
    val tag = CACHE_ADDR_SIZE - index - offset
    (tag, index, offset, numOfSets)
  }
  
  // cache Structure (valid , tag)
  var cacheStruct = CacheStruct(n, k)
  
  // LRU
  var LRU = Array.fill[Int](n, k)(0)
  var LRU_i = Array.fill[Int](n)(0)
  var LRU_j = Array.fill[Int](n)(0)
  var LRU_max = Array.fill[Int](n)(0)
  
  // FIFO
  
  
  // Bit masks
  val tagBitMask = (pow(2, tagBit).toLong - 1) << (indexBit + offsetBit)
  val indexBitMask = (pow(2, indexBit).toLong - 1) << (offsetBit)
  val offsetBitMask = (pow(2, offsetBit).toLong - 1)
  
  // buffer
  val streamBuffer:Array[StreamBuffer] = {
    if (level == "L3") {
      Array.fill[StreamBuffer](512)(StreamBuffer(10))
    } else {
      null
    }
  }
  
  def access(accessType:Int, content:Long):Unit = {
    totalCount += 1
    
    var contentIndex= ( ( content & indexBitMask ) >>> offsetBit ).toInt
//    var contentTag = ( content & tagBitMask) >>> (indexBit + offsetBit)
    var contentTag = ( content ) >>> (indexBit + offsetBit)
 		
	  // Increment LRU count having proper contentIndex
	  LRU(contentIndex) = LRU(contentIndex).map(_+1)
	  
	  // generate LRU Max and index(i,j)
    getLRUMax(contentIndex)
  
    // 1) HIT case
    //  Find proper block index:n | associative:k(iteration) | (valid = true & tag is the same)
    val res = cacheStruct.find(contentIndex, contentTag) 
    if (res._1 == true) {
      incrHitCount
      LRU(contentIndex)(res._2) = 0
    	return 
    }
    // 2) MISS case
    // 2-1) L3: prefetch(victim=random) + {LRU or FIFO}
    if (level == "L3") {
      // LRU
      if (LRU_OR_FIFO == true) {
      	LRU(contentIndex)(LRU_j(contentIndex)) = 0
        val res = checkBuffer(contentTag)
       	if (res._1 == true) {
       	  cacheStruct.fetch(LRU_i(contentIndex), LRU_j(contentIndex), contentTag)
      	  this.streamBuffer(res._2).preFetch(contentTag)
          incrHitCount
        } else {
      	  cacheStruct.fetch(LRU_i(contentIndex), LRU_j(contentIndex), contentTag)
      	  val index = findIndex
          this.streamBuffer(index).preFetchAll(contentTag)
          incrMissCount(accessType)
        }
      }
      // FIFO
      else {
        
      }
    } 
    // 2-2) L1,L2: fetch + LRU
    else if (level == "L1" || level == "L2"){
    	LRU(contentIndex)(LRU_j(contentIndex)) = 0
    	cacheStruct.fetch(LRU_i(contentIndex), LRU_j(contentIndex), contentTag)
    	incrMissCount(accessType)
    	this.subCache.access(accessType, content)
    }
  }
  
  def getLRUMax(contentIndex:Int) = {
    if ( LRU_max(contentIndex) < LRU(contentIndex).max ) {
    	LRU_max(contentIndex) = LRU(contentIndex).max
    	LRU_i(contentIndex) = contentIndex
    	LRU_j(contentIndex) = LRU(contentIndex).indexOf(LRU(contentIndex).max)
    }
  }
  
  def checkBuffer(contentTag:Long) = {
    val res = streamBuffer.zipWithIndex.filter(x => (x._1.checkContains(contentTag) == true))
                          .map(x => x._2)
    if (res.isEmpty) {
      (false, -1)
    } else {
      (true, res(0)) // return streamBuffer index
    }
  }
  
  def findIndex() = {
    val res = streamBuffer.zipWithIndex.filter(x => (x._1.findEmptyBuffer() == true))
                          .map(x => x._2)
    if (res.isEmpty) {
      // Do not have empty buffer
      // So, return random index
//      println("fullAll")
      Random.nextInt(512)
    } else {
      res(0)
    }
  }
  
  def incrHitCount() {
    this.hitCount += 1
  }
  
  def incrMissCount(accessType:Int) {
    this.missCount += 1
    accessType match {
      case DATA_READ => this.dataReadMissCount += 1
      case DATA_WRITE => this.dataWriteMissCount += 1
      case INSTR_READ => this.instrReadMissCount += 1
    }
  }
  
  def setSubCache(cache:Cache):Unit = {
    this.subCache = cache
  }
  
  def printStatistics(name:String):Unit = {
		  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
			  val p = new java.io.PrintWriter(f)
					  try { op(p) } finally { p.close() }
		  }
			import java.io.File
			printToFile(new File(PATH_PREFIX+RESULT_PREFIX + name+"_"+identifier.split(" ")(1).substring(0,1)+".txt")) { p =>
			  p.println("Name :"+identifier)
			  p.println("TotalCount : "+totalCount)
			  p.println("hitCount : "+hitCount)
			  p.println("missCount : "+missCount)
			  p.println("InstrReadMissCount : "+instrReadMissCount)
			  p.println("dataReadMissCount : "+dataReadMissCount)
			  p.println("dataWriteMissCount : "+dataWriteMissCount)
			  p.println()
		  }
  }
}
case class CacheStruct (n:Int, k:Int) {
  var data = Array.fill[(Boolean,Long)](n,k)(false, -1)
  
  def find(contentIndex:Int, contentTag:Long):(Boolean, Int) = {
    	// (1) check valid
    val list = List.range(0, k)
    var res = list.filter(index => data(contentIndex)(index)._1 == true && data(contentIndex)(index)._2 == contentTag)
    if (res.size > 0) {
    	// Find proper set := return index for initializing LRU count
    	(true, res(0))
    } else {
    	// Cannot find proper set
    	(false, -1)
    }
  }
	def fetch(i:Int, j:Int, contentTag:Long) {
    this.data(i)(j) = (true, contentTag)
  }
}

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
