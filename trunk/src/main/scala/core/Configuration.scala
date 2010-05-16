package core 
import scala.xml.{Elem,NodeSeq}
import scala.xml.XML
import util.schedulingQueue

class Configuration(pathToFile:String){
  val xmlConfiguration:Elem = XML.load(new java.io.FileInputStream(new java.io.File(pathToFile)))
  /**
   * File system
   */
  val fileSystemFile:String = (xmlConfiguration \\ "fs" \ "@path").toString
    
  /**
   * Directories 
   */
  val directoriesXMLElement:NodeSeq = xmlConfiguration \\ "directories"
  val pathToRootDirectory:String = (directoriesXMLElement \\ "root" \ "@path").toString
  val pathToLogDirectory:String = (directoriesXMLElement \\ "logs" \ "@path").toString

  /**
   * Memory
   */
  val memoryXMLElement:NodeSeq = xmlConfiguration \\ "memory"
  val ramSize = (memoryXMLElement \ "@ram").text.toInt
  val swapSize = (memoryXMLElement \ "@swap").text.toInt
  val frameSize = (memoryXMLElement \ "@page").text.toInt

  /**
   * Scheduling
   */
  val schedulingXMLElement:NodeSeq = xmlConfiguration \\ "scheduling"
  val startQueue = (schedulingXMLElement \ "@startQueue").text.toInt
  val queuesXMLElement = schedulingXMLElement \\ "queue"
  val nQueues:Int = queuesXMLElement.size

//  bootLog.info("Found "+nQueues.toString+" scheduling queues in configuration file:")

  //var queues:List[schedulingQueue] = List()
  val queues:Seq[schedulingQueue] = for {
    schedulingQueueElem <- queuesXMLElement
    val id:Int = (schedulingQueueElem \ "@id").text.toInt
    val name = (schedulingQueueElem \ "@name").text
    val typeOfQueue = (schedulingQueueElem \ "@type").text
    val quantum:Int = (schedulingQueueElem \ "@quantum").text.toInt
    val preemptive:Boolean = (schedulingQueueElem \ "@preemptive").text match{
      case "0" => false
      case _ => true
    }
    val queueOnQuantumEnd:Int = (schedulingQueueElem \ "@queueOnQuantumEnd").text.toInt
    val queueOnIOEnd:Int = (schedulingQueueElem \ "@queueOnIOEnd").text.toInt

    val newQueue = new schedulingQueue(id,name,typeOfQueue,quantum,preemptive,queueOnQuantumEnd,queueOnIOEnd)
  } yield newQueue

  override def toString:String = {
    "\tMemory: \n"+
    "\t\tRam size: "+ramSize+"\n"+
    "\t\tSwap size: "+swapSize+"\n"+
    "\t\tFrame size: "+frameSize+"\n"+
    "\tScheduling: \n"+
    "\t\tStart queue: "+startQueue+"\n"+
    "\t\tNumber of queues Loaded: "+nQueues+"\n"+
    "\t\tQueues: "+queues.mkString(",")
  }
} //end class Configuration
