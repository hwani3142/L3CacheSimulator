package cacheSimulator

import scala.io.Source
import scala.math.log10
import java.io.{FileNotFoundException, IOException}
import cacheSimulator.ConstantObject.{INST_READ, BYTE_PER_LINE}

object main {
  def main(args:Array[String]):Unit = {
		 val source = Source.fromFile(ConstantObject.TRACE1_PATH)
    // Get trace content from source
    try {
      val lines = source.getLines
      val content = lines.map { x => 
        val splitX = x.split(" ")
        (splitX(0).toInt, splitX(1).toLong)
      }.toList
      
      val L1_Instruction  = Cache("L1 Instruction", BYTE_PER_LINE, 1, 32)
      val L1_Data         = Cache("L1 Data", BYTE_PER_LINE, 1, 32)
      val L2              = Cache("L2", BYTE_PER_LINE, 8, 256)
//      val L3              = Cache("L3", BYTE_PER_LINE, 8, 256)
      
      L1_Instruction.setSubCache(L2)
      L1_Data.setSubCache(L2)
      
      content.foreach{ x =>
        x._1 match {
        case INST_READ => L1_Instruction.access(x._1, x._2)
        case _ => L1_Data.access(x._1, x._2)
        }
      }
      
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
  val PATH_PREFIX = "/home/hwan/workspace/ca-project/src/main/resources/"
  val TRACE1_PATH = PATH_PREFIX + "Trace1"
  val TRACE2_PATH = PATH_PREFIX + "Trace2"
  
  // Data format
  val DATA_READ = 0
  val DATA_WRITE = 1
  val INST_READ = 2
  
  val CACHE_ADDR_SIZE = 32
  val BYTE_PER_LINE = 256 // L
  val WORD_SIZE = 4
  
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