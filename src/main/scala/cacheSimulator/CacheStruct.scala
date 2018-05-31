package cacheSimulator

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