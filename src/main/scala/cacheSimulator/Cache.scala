package cacheSimulator

import cacheSimulator.ConstantObject.{WORD_SIZE, KB, log2,
                                     DATA_READ, DATA_WRITE, INSTR_READ,
                                      CACHE_ADDR_SIZE, PATH_PREFIX, RESULT_PREFIX}
import scala.math.pow

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
  
  // LRU count
  var LRU = Array.fill[Int](n, k)(0)
  var LRU_i = Array.fill[Int](n)(0)
  var LRU_j = Array.fill[Int](n)(0)
  var LRU_max = Array.fill[Int](n)(0)
  
  // Bit masks
  val tagBitMask = (pow(2, tagBit).toLong - 1) << (indexBit + offsetBit)
  val indexBitMask = (pow(2, indexBit).toLong - 1) << (offsetBit)
  val offsetBitMask = (pow(2, offsetBit).toLong - 1)
  
  def access(accessType:Int, content:Long):Unit = {
//		  if (this.identifier == "L2 2") {
//		  println(tagBit)
//		  println(indexBit)
//		  println(offsetBit)
//    println(tagBitMask.toBinaryString + " , " + tagBitMask)
//    println(indexBitMask.toBinaryString + " , " + indexBitMask)
//    println(offsetBitMask.toBinaryString)
//		  }
    totalCount += 1
    
    var contentIndex= ( ( content & indexBitMask ) >>> offsetBit ).toInt
//    var contentTag = ( content & tagBitMask) >>> (indexBit + offsetBit)
    var contentTag = ( content ) >>> (indexBit + offsetBit)
 		
	  // Increment LRU count having proper contentIndex
	  LRU(contentIndex) = LRU(contentIndex).map(_+1)
	  
	  // Set maximum LRU count, i, j
//	  if (this.identifier == "L2 2") {
//	  println("toLong :"+contentTag)
//	  println("toInt:"+contentTag.toInt)
//	  println("toLong :"+contentIndex)
//	  println("toInt:"+contentIndex.toInt)
//	  println("toBinaryString:"+contentTag.toBinaryString)
//	  println("toBinaryString:"+content.toBinaryString)
//	  }
	  
//	  println("toBinaryString:"+ (content & tagBitMask) + ", " + tagBitMask)
	  if ( LRU_max(contentIndex) < LRU(contentIndex).max ) {
	    LRU_max(contentIndex) = LRU(contentIndex).max
	    LRU_i(contentIndex) = contentIndex
	    LRU_j(contentIndex) = LRU(contentIndex).indexOf(LRU(contentIndex).max)
//  	  if (this.identifier == "L2 2") {
//  	    println(s"max $LRU_max")
//  	  }
	  }
  
    // 1) HIT case
    //  Find proper block index:n | associative:k(iteration) | (valid = true & tag is the same)
    val res = cacheStruct.find(contentIndex, contentTag) 
    if (res._1 == true) {
      hitCount += 1
      LRU(contentIndex)(res._2) = 0
    	return 
    }
    // 2) MISS case
    missCount += 1
    LRU(contentIndex)(LRU_j(contentIndex)) = 0
    accessType match {
      case DATA_READ => dataReadMissCount += 1
      case DATA_WRITE => dataWriteMissCount += 1
      case INSTR_READ => instrReadMissCount += 1
    }
    cacheStruct.setNewPosition(LRU_i(contentIndex), LRU_j(contentIndex), contentTag)
    
    if (this.subCache != null) {
      this.subCache.access(accessType, content)
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
	def setNewPosition(i:Int, j:Int, contentTag:Long) {
//	  if (identifier == "L2 2" && i == 0) {
//	    println(s"$i , $j / prev: ${this.data(i)(j)} , cur: $contentTag")
//	  }
    this.data(i)(j) = (true, contentTag)
  }
}
