package cacheSimulator

import cacheSimulator.ConstantObject.{WORD_SIZE, KB, 
                                      log2, CACHE_ADDR_SIZE}

case class Cache 
  (identifier:String, 
      l:Integer, k:Integer, size:Integer){
  
  // Counting value
  var totalCount = 0
  var hitCount = 0
  var missCount = 0
  
  var instrReadMissCount = 0
  var dataReadMissCount = 0
  var writeMissCount = 0
  
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
  
  var cacheAddress = { 
	  val temp = Array.ofDim[Integer](n, k)
	  for {
	    i <- 0 to n
	    j <- 0 to k
	  } {
	    
	  }
  }
  var LRU = Array.ofDim[Integer](n, k)
  
  def access(accessType:Integer, content:Long):Unit = {
    // 1) HIT case
    
    // 2) MISS case
    // Get LRU
		  for {
			  i <- 0 to n
			  j <- 0 to k
		  } {
		    
		  }
  }
  
//  def setCacheBit():Unit = {
//    val blockSize = L * WORD_SIZE
//    val size = this.size * KB
//    val numOfBlocks = size / blockSize
//    val numOfSets = numOfBlocks / this.K
//    offsetBit = log2(L)
//    indexBit = log2(numOfSets)
//    tagBit = CACHE_ADDR_SIZE - indexBit - offsetBit
//  }
  def setSubCache(cache:Cache):Unit = {
    this.subCache = cache
  }
  
  def getStatistics():Unit = {
    
  }
}