package cacheSimulator

import cacheSimulator.ConstantObject.{WORD_SIZE, KB, 
                                     DATA_READ, DATA_WRITE, INSTR_READ,
                                      log2, CACHE_ADDR_SIZE}
import scala.math.pow

case class Cache 
  (identifier:String, 
      l:Int, k:Int, n:Int){
  
  // Counting value
  var totalCount = 0
  var hitCount = 0
  var missCount = 0
  
  var instrReadMissCount = 0
  var dataReadMissCount = 0
  var dataWriteMissCount = 0
  
  // sub cache
  var subCache:Cache = _
 
  val (tagBit, indexBit, offsetBit) = {
//    val blockSize = l * WORD_SIZE
//    val size = this.size * KB
//    val numOfBlocks = size / blockSize
//    val numOfSets = numOfBlocks / this.k
    val offset = log2(l)
    val index = log2(n)
    val tag = CACHE_ADDR_SIZE - index - offset
    (tag, index, offset)
  }
  
  // cache Structure (valid , tag)
  var cacheStruct = CacheStruct(n, k)
  
  // LRU count
  var LRU = Array.fill[Int](n, k)(0)
  
  // Bit masks
  val tagBitMask = (pow(2, tagBit).toLong - 1) << (indexBit + offsetBit)
  val indexBitMask = (pow(2, indexBit).toLong - 1) << (offsetBit)
  val offsetBitMask = (pow(2, offsetBit).toLong - 1)
  
  def access(accessType:Int, content:Long):Unit = {
//		  println(tagBit)
//		  println(indexBit)
//		  println(offsetBit)
//    println(tagBitMask.toBinaryString)
//    println(indexBitMask.toBinaryString)
//    println(offsetBitMask.toBinaryString)
    totalCount += 1
    
 		var contentIndex= ( content & indexBitMask ) >> offsetBit
 		var contentTag = ( content & tagBitMask) >> (indexBit + offsetBit)
 		
	  var LRU_i = 0
	  var LRU_j = 0
	  var LRU_max = 0
	  // Increment all LRU count
	  LRU = LRU.map ( x =>  x.map(_+1) )
	  // Set maximum LRU count, i, j
//	  println("toLong :"+contentIndex)
//	  println("toInt:"+contentIndex.toInt)
	  if ( LRU_max < LRU(contentIndex.toInt).max ) {
	    LRU_max = LRU(contentIndex.toInt).max
	    LRU_i = contentIndex.toInt
	    LRU_j = LRU(contentIndex.toInt).indexOf(LRU(contentIndex.toInt).max)
	  }
  
    // 1) HIT case
    //  Find proper block index:n | associative:k(iteration) | (valid = true & tag is the same)
    val res = cacheStruct.find(contentIndex.toInt, contentTag.toInt) 
    if (res._1 == true) {
      LRU(contentIndex.toInt)(res._2) = 0
    	 return 
    }
    // 2) MISS case
    accessType match {
      case DATA_READ => dataReadMissCount += 1
      case DATA_WRITE => dataWriteMissCount += 1
      case INSTR_READ => instrReadMissCount += 1
    }
    cacheStruct.setNewPosition(LRU_i, LRU_j, contentTag.toInt)
  }
  def setSubCache(cache:Cache):Unit = {
    this.subCache = cache
  }
  def printStatistics():Unit = {
		  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
			  val p = new java.io.PrintWriter(f)
					  try { op(p) } finally { p.close() }
			  import java.io._
			  printToFile(new File("/home/hwan/workspace/ca-project/results.txt")) { p =>
			    p.println("Name :"+identifier)
			    p.println("TotalCount : "+totalCount)
			    p.println("InstrReadMissCount : "+instrReadMissCount)
			    p.println("dataReadMissCount : "+dataReadMissCount)
			    p.println("dataWriteMissCount : "+dataWriteMissCount)
			    p.println()
			  }
		  }
  }
}
case class CacheStruct (n:Int, k:Int) {
  var valid = Array.fill[Boolean](n, k)(false)
  var tag =   Array.fill[Int](n, k)(-1)
  
  def find(contentIndex:Int, contentTag:Int):(Boolean, Int) = {
    	// (1) check valid
    	var filteredValid  = this.valid(contentIndex).zipWithIndex.filter(x => (x._1 == true)).map( x => x._2)
    	// (2) check tag
    	var filteredTag = this.tag(contentIndex).zipWithIndex.filter(x => (filteredValid.contains(x._2)) && (x._1 == contentTag))
    	if (filteredTag.size > 0) {
    		// Find proper set := return index for initializing LRU count
    	  (true, filteredTag.map(x => x._2).mkString.toInt)
    	} else {
    		// Cannot find proper set
    	  (false, -1)
    	}
  }
  def setNewPosition(i:Int, j:Int, contentTag:Int) {
    this.valid(i)(j) = true
    this.tag(i)(j) = contentTag
  }
  
}
