package cacheSimulator

import scala.io.Source
import scala.math.log10
import java.io.{FileNotFoundException, IOException}
import cacheSimulator.ConstantObject.{INSTR_READ, BYTE_PER_LINE, NUMBER_OF_SETS, ASSOCIATIVE}

object main {
  def main(args:Array[String]):Unit = {
		 val source = Source.fromFile(ConstantObject.TRACE1_PATH)
    // Get trace content from source
    try {
      
//      val L1_Instruction  = Cache("L1 Instruction", BYTE_PER_LINE, ASSOCIATIVE, NUMBER_OF_SETS)
//      val L1_Data         = Cache("L1 Data", BYTE_PER_LINE, ASSOCIATIVE, NUMBER_OF_SETS)
      val L1_Instruction  = Cache("L1 Instruction", BYTE_PER_LINE, ASSOCIATIVE, 64)
      val L1_Data         = Cache("L1 Data", BYTE_PER_LINE, ASSOCIATIVE, 64)
      val L2              = Cache("L2", BYTE_PER_LINE, 8, 256)
//      val L3              = Cache("L3", BYTE_PER_LINE, 8, 256)
      
      L1_Instruction.setSubCache(L2)
      L1_Data.setSubCache(L2)
      
      
      import java.util.Calendar
      import java.text.SimpleDateFormat
      val time = Calendar.getInstance.getTime
      val name = {
    		  val month = new SimpleDateFormat("MM")
    				  val day = new SimpleDateFormat("dd")
    				  val hour = new SimpleDateFormat("hh")
    				  val min = new SimpleDateFormat("mm")
    				  month.format(time) + day.format(time) + hour.format(time) + min.format(time)
      }
      
      println("Start all access")
      val lines = source.getLines
      var count = 0
      lines.foreach { x => 
//      println(x)
        count += 1
        val splitX = x.split(" ")
        val accessType = splitX(0).toInt
        val addressContent = java.lang.Long.decode(splitX(1))
        accessType match {
          case INSTR_READ => L1_Instruction.access(accessType, addressContent)
          case _ => L1_Data.access(accessType, addressContent)
         }
        if (count % 10000 == 0)  {
          println(count + "("+ (count*1.0/100000)+")")
        }
       }
      println("Finish all access")
      
      println("Start print statistics")
      L1_Instruction.printStatistics(name)
      L1_Data.printStatistics(name)
      println("Finishprint statistics")
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
  val TRACE1_PATH = PATH_PREFIX + "Trace1"
  val TRACE2_PATH = PATH_PREFIX + "Trace2"
  
  // Data format
  val DATA_READ = 0
  val DATA_WRITE = 1
  val INSTR_READ = 2
  
  val CACHE_ADDR_SIZE = 64
  val BYTE_PER_LINE = 64 // L
  val ASSOCIATIVE = 1024 // K
  val NUMBER_OF_SETS = 4096 // N
  val WORD_SIZE = 1
  
  // Access latency (cycles)
  val L1_INST_LATENCY = 4
  val L1_DATA_LATENCY = 4
  val L2_LATENCY = 16
  val L3_LATENCY = 32
  val MEMORY_LATENCY = 120
  
  // 
  val KB = 1024
  // logarithm base 2
  var log2 = (x: Integer) => (log10(x.toDouble)/log10(2.0)).toInt
}