package cacheSimulator

import cacheSimulator.ConstantObject.{WORD_SIZE, KB, 
                                     DATA_READ, DATA_WRITE, INSTR_READ,
                                      log2, CACHE_ADDR_SIZE}
import scala.math.pow

case class Cache 
  (identifier:String, 
//      l:Int, k:Int, n:Int){
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
 
//  val (tagBit, indexBit, offsetBit) = {
		  val (tagBit, indexBit, offsetBit, n) = {
    val blockSize = l * WORD_SIZE
    val size = this.size * KB
    val numOfBlocks = size / blockSize
    val numOfSets = numOfBlocks / this.k
    val offset = log2(l)
//    val index = log2(n)
    val index = log2(numOfSets)
    val tag = CACHE_ADDR_SIZE - index - offset
    (tag, index, offset, numOfSets)
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
//		  println("k : " + k + " , n: " + n)
//		  println(tagBit)
//		  println(indexBit)
//		  println(offsetBit)
//    println(tagBitMask.toBinaryString + " , " + tagBitMask)
//    println(indexBitMask.toBinaryString)
//    println(offsetBitMask.toBinaryString)
    totalCount += 1
    
 		var contentIndex= ( content & indexBitMask ) >>> offsetBit
 		var contentTag = ( content ) >>> (indexBit + offsetBit)
//  var contentIndex= ( content & indexBitMask ) >>> offsetBit
//  var contentTag = ( content & tagBitMask) >>> (indexBit + offsetBit)
 		
	  var LRU_i = 0
	  var LRU_j = 0
	  var LRU_max = 0
	  // Increment LRU count having proper contentIndex
//	  LRU = LRU.map ( x =>  x.map(_+1) )
	  LRU(contentIndex.toInt) = LRU(contentIndex.toInt).map(_+1)
	  
	  // Set maximum LRU count, i, j
//	  println("toLong :"+contentTag)
//	  println("toInt:"+contentTag.toInt)
//	  println("toLong :"+contentIndex)
//	  println("toInt:"+contentIndex.toInt)
//	  println("toBinaryString:"+contentTag.toBinaryString)
//	  println("toBinaryString:"+content.toBinaryString)
//	  println("toBinaryString:"+ (content & tagBitMask) + ", " + tagBitMask)
	  if ( LRU_max < LRU(contentIndex.toInt).max ) {
	    LRU_max = LRU(contentIndex.toInt).max
	    LRU_i = contentIndex.toInt
	    LRU_j = LRU(contentIndex.toInt).indexOf(LRU(contentIndex.toInt).max)
	  }
  
    // 1) HIT case
    //  Find proper block index:n | associative:k(iteration) | (valid = true & tag is the same)
    val res = cacheStruct.find(contentIndex.toInt, contentTag.toInt) 
    if (res._1 == true) {
      hitCount += 1
      LRU(contentIndex.toInt)(res._2) = 0
    	return 
    }
    // 2) MISS case
    accessType match {
      case DATA_READ => dataReadMissCount += 1
      case DATA_WRITE => dataWriteMissCount += 1
      case INSTR_READ => instrReadMissCount += 1
    }
    missCount += 1
    cacheStruct.setNewPosition(LRU_i, LRU_j, contentTag.toInt)
    
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
			printToFile(new File("C:\\test\\"+name+"_"+identifier.split(" ")(1).substring(0,1)+".txt")) { p =>
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
  var data = Array.fill[(Boolean,Int)](n,k)(false, -1)
//  var valid = Array.fill[Boolean](n, k)(false)
//  var tag =   Array.fill[Int](n, k)(-1)
  
  def find(contentIndex:Int, contentTag:Int):(Boolean, Int) = {
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
  def setNewPosition(i:Int, j:Int, contentTag:Int) {
    this.data(i)(j) = (true, contentTag)
  }
  
}
