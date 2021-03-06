package cacheSimulator

import scala.io.Source
import scala.math.log10
import java.io.{FileNotFoundException, IOException}
import cacheSimulator.ConstantObject._

object main {
  def main(args:Array[String]):Unit = {
		val source = Source.fromFile(PATH_PREFIX + TRACE1)
    // Get trace content from source
    try {
      val L1_Instruction  = Cache("L1 Instruction", BYTE_PER_LINE, L1_ASSOCIATIVE, L1_SIZE) // n = 128
      val L1_Data         = Cache("L1 Data", BYTE_PER_LINE, L1_ASSOCIATIVE, L1_SIZE)
      val L2              = Cache("L2 2", BYTE_PER_LINE, L2_ASSOCIATIVE, L2_SIZE) // n = 128
      val L3              = Cache("L3 3", BYTE_PER_LINE, L3_ASSOCIATIVE, L3_SIZE)
      
      L1_Instruction.setSubCache(L2)
      L1_Data.setSubCache(L2)
      L2.setSubCache(L3)
      
      import java.util.Calendar
      import java.text.SimpleDateFormat
      val time = Calendar.getInstance.getTime
      val name = {
    		  val month = new SimpleDateFormat("MM")
    				  val day = new SimpleDateFormat("dd")
    				  val hour = new SimpleDateFormat("HH")
    				  val min = new SimpleDateFormat("mm")
    				  month.format(time) + day.format(time) + hour.format(time) + min.format(time)
      }
      
      println("Start all access")
      val lines = source.getLines
      var count = 0
      lines.foreach { x => 
        count += 1
        val splitX = x.split(" ")
        val accessType = splitX(0).toInt
        val addressContent = java.lang.Long.decode(splitX(1))
        accessType match {
          case INSTR_READ => L1_Instruction.access(accessType, addressContent)
          case _ => L1_Data.access(accessType, addressContent)
         }
        if (count % 500000 == 0)  {
          println(count + " ("+ (count*1.0/100000)+"%)")
        }
       }
      println("Finish all access")
      
      println("Start print statistics")
      L1_Instruction.printStatistics(name)
      L1_Data.printStatistics(name)
      L2.printStatistics(name)
      L3.printStatistics(name)
      println("Finish print statistics")
    } catch {
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException!")
    } finally {
    	source.close
    }
  }
}

object ConstantObject {
  // Trace file path
//  val PATH_PREFIX = "/home/hwan/workspace/ca-project/src/main/resources/"
	val PATH_PREFIX = "C:\\test\\"
	val RESULT_PREFIX = "projectResult\\"
  val TRACE1 = "Trace1"
  val TRACE2 = "Trace2"
  
  // Data format
  val DATA_READ = 0
  val DATA_WRITE = 1
  val INSTR_READ = 2
  
  val WORD_SIZE = 1
  val CACHE_ADDR_SIZE = 64
  
  val L1 = "L1"
  val L2 = "L2"
  val L3 = "L3"
  
  val BYTE_PER_LINE = 64 // L
  val L1_ASSOCIATIVE = 1 // L1 K
  val L2_ASSOCIATIVE = 8 // L2 K
//  val L3_ASSOCIATIVE = 4096 // L3 K
  val L3_ASSOCIATIVE = 2048 // L3 K
  
  val L1_SIZE = 32 //32KB
  val L2_SIZE = 256 // 256KB
  val L3_SIZE = 2048 // 2MB
  val KB = 1024
//  val NUMBER_OF_SETS = 4096 // N
  
  // Access latency (cycles)
  val L1_INST_LATENCY = 4
  val L1_DATA_LATENCY = 4
  val L2_LATENCY = 16
  val L3_LATENCY = 32
  val MEMORY_LATENCY = 120
  
//  val LRU_OR_LFU = true // LRU
  val LRU_OR_LFU = false // LFU
  val STREAMBUFFER_SIZE = 10
  val NUM_OF_STREAMBUFFER = 100
//  val NUM_OF_STREAMBUFFER = 100
  val BUFFER_RANDOM_OR_LFU = true // Random
//  val BUFFER_RANDOM_OR_LFU = false // LFU
  
  // logarithm base 2
  var log2 = (x: Integer) => (log10(x.toDouble)/log10(2.0)).toInt
}