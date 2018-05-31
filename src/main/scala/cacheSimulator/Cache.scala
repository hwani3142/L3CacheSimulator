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
 
  // # of Bit and N
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
  // Bit masks
  val tagBitMask = (pow(2, tagBit).toLong - 1) << (indexBit + offsetBit)
		  val indexBitMask = (pow(2, indexBit).toLong - 1) << (offsetBit)
		  val offsetBitMask = (pow(2, offsetBit).toLong - 1)
  
  // cache Structure (valid , tag)
  var cacheStruct = CacheStruct(n, k)
  
  // LRU
  var LRU = Array.fill[Int](n, k)(0)
  var LRU_i = Array.fill[Int](n)(0)
  var LRU_j = Array.fill[Int](n)(0)
  var LRU_max = Array.fill[Int](n)(0)
  
  // LFU
  var LFU = Array.fill[Int](n, k)(0)
  var LFU_i = Array.fill[Int](n)(0)
  var LFU_j = Array.fill[Int](n)(0)
  var LFU_min = Array.fill[Int](n)(0)
  
  
  
  // buffer
  val streamBuffer:Array[StreamBuffer] = {
    if (level == L3) Array.fill[StreamBuffer](NUM_OF_STREAMBUFFER)(StreamBuffer(STREAMBUFFER_SIZE))
    else null

  }
  // buffer LFU
  var buffer_LFU = Array.fill[Int](NUM_OF_STREAMBUFFER)(0)
  var buffer_LFU_i = 0
  var buffer_LFU_min = 0
  
  def access(accessType:Int, content:Long):Unit = {
    totalCount += 1
    
 		var contentTag = ( content ) >>> (indexBit + offsetBit)
    var contentIndex= ( ( content & indexBitMask ) >>> offsetBit ).toInt
//    var contentTag = ( content & tagBitMask) >>> (indexBit + offsetBit)
 		
    // use LRU
    if (level == L1 || level == L2 || LRU_OR_LFU == true) {
	    // Increment LRU count having proper contentIndex
	    LRU(contentIndex) = LRU(contentIndex).map(_+1)
	    // generate LRU Max and index(i,j)
      getLRUMax(contentIndex)
    }
    // use LFU
    else {
      getLFUMin(contentIndex)
    }
  
    // 1) HIT case
    //  Find proper block index:n | associative:k(iteration) | (valid = true & tag is the same)
    val res = cacheStruct.find(contentIndex, contentTag) 
    if (res._1 == true) {
      incrHitCount
      if (level == L1 || level == L2 || LRU_OR_LFU == true) {
        LRU(contentIndex)(res._2) = 0
      }
      // level L3 (LFU)
      else {
        LFU(contentIndex)(res._2) += 1
      }
    	return 
    }
    
    // 2) MISS case
    // 2-1) L3: prefetch(victim=random) + {LRU or FIFO}
    if (level == L3) {
      // LRU
      if (LRU_OR_LFU == true) {
      	LRU(contentIndex)(LRU_j(contentIndex)) = 0
        val res = checkBuffer(contentTag)
       	if (res._1 == true) {
       	  // additional hit
       	  cacheStruct.fetch(LRU_i(contentIndex), LRU_j(contentIndex), contentTag)
       	  buffer_LFU(res._2) += 1
      	  this.streamBuffer(res._2).preFetch(contentTag)
          incrHitCount
        } else {
        	// miss
      	  cacheStruct.fetch(LRU_i(contentIndex), LRU_j(contentIndex), contentTag)
          this.streamBuffer(findIndex).preFetchAll(contentTag)
          incrMissCount(accessType)
        }
      }
      // LFU
      else {
        LFU(contentIndex)(LFU_j(contentIndex)) = 1
        val res = checkBuffer(contentTag)
       	if (res._1 == true) {
       	  // additional hit
       	  cacheStruct.fetch(LFU_i(contentIndex), LFU_j(contentIndex), contentTag)
       	  // buffer LFU
       	  buffer_LFU(res._2) += 1
      	  this.streamBuffer(res._2).preFetch(contentTag)
          incrHitCount
        } else {
        	// miss
      	  cacheStruct.fetch(LFU_i(contentIndex), LFU_j(contentIndex), contentTag)
          this.streamBuffer(findIndex).preFetchAll(contentTag)
          incrMissCount(accessType)
        }
      }
    } 
    // 2-2) L1,L2: fetch + LRU
    else if (level == L1 || level == L2){
    	LRU(contentIndex)(LRU_j(contentIndex)) = 0
    	cacheStruct.fetch(LRU_i(contentIndex), LRU_j(contentIndex), contentTag)
    	incrMissCount(accessType)
    	this.subCache.access(accessType, content)
    }
  }
  
  def getLFUMin(contentIndex:Int) = {
   	LFU_min(contentIndex) = LFU(contentIndex).min
   	LFU_i(contentIndex) = contentIndex
   	LFU_j(contentIndex) = LFU(contentIndex).indexOf(LFU(contentIndex).min)
//   	if (contentIndex == 0)	print("("+LFU_j(contentIndex)+","+LFU_min(contentIndex)+")")
  }
  
  def getLRUMax(contentIndex:Int) = {
    LRU_max(contentIndex) = LRU(contentIndex).max
    LRU_i(contentIndex) = contentIndex
    LRU_j(contentIndex) = LRU(contentIndex).indexOf(LRU(contentIndex).max)
  }
  
  def checkBuffer(contentTag:Long) = {
    val res = streamBuffer.zipWithIndex.filter(x => (x._1.checkContains(contentTag) == true))
                          .map(x => x._2) // index
    if (res.isEmpty) {
      (false, -1)
    } else {
      (true, res(0)) // return streamBuffer index
    }
  }
  
  def findIndex() = {
    val emptyBufferIndex = streamBuffer.zipWithIndex.filter(x => (x._1.findEmptyBuffer() == true))
                                        .map(x => x._2)
    if (emptyBufferIndex.isEmpty) {
      // Do not have empty buffer. So, return random or LFU index
      if (BUFFER_RANDOM_OR_LFU) {
    	  val randomIndex = Random.nextInt(NUM_OF_STREAMBUFFER)
    	  randomIndex
      } else {
        // LFU
       	buffer_LFU_min = buffer_LFU.min
   			buffer_LFU_i = buffer_LFU.indexOf(buffer_LFU_min)
        buffer_LFU(buffer_LFU_i) = 1
        buffer_LFU_i // return min's index
      }
    } else {
      buffer_LFU(emptyBufferIndex(0)) += 1
      emptyBufferIndex(0)
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