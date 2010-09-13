package core 

import programs.program
import scala.collection.immutable.{ListSet}
import scala.collection.mutable.{Queue,ArrayBuffer,Buffer}
import exceptions._
import util.{schedulingQueue,tickMeta,Log}
import java.io.File

/**
 * NOTA: Arreglar queue
 */
class OperatingSystem(pathToConfigurationFile:String){
  /**
   * Load sys.xml file
   */
  val configuration = new Configuration(pathToConfigurationFile)
  val pathToFileSystemFile:String = configuration.fileSystemFile
  val pathToHome:String = configuration.pathToHomeDirectory
  //if pathToHome doesn't exist, create it
  if (!new File(pathToHome).exists)
    new File(pathToHome).mkdirs

  val fs = new FileSystem(pathToFileSystemFile)
  val pathToLogs = configuration.pathToLogDirectory
  //if pathToLogs dir doesn't exist, create it 
  if (!new File(pathToHome).exists)
    new File(pathToLogs).mkdirs

  var isON = true

  //create boot log
  val bootLog = new util.Log(pathToLogs+"boot.log")
  bootLog.info("Configuration loaded: \n"+configuration.toString)
  
  val registers = scala.collection.mutable.Map[String,Int](
    "R0" -> 0, "R1" -> 0, "R2" -> 0, "R3" -> 0, "R4" -> 0, "R5" -> 0, "R6" -> 0, "R7" -> 0,
    "R8" -> 0, "R9" -> 0, "R10" -> 0, "R11" -> 0, "R12" -> 0, "R13" -> 0, "R14" -> 0, "R15" -> 0
  )

  /**
   * Create memory and memory log
   */
  val ramSize = configuration.ramSize
  val pageSize = configuration.frameSize
  val swapSize = configuration.swapSize

  val memLog = new Log(pathToLogs+"mem.log")
  memLog.info("Mem log initialized")

  val pageOwners:Buffer[Int] = Buffer.fill(ramSize)(-1)
  val memory:Buffer[Buffer[Int]] = Buffer.fill(ramSize)(Buffer.fill(pageSize)(0))
  val swap:Buffer[Buffer[Int]] = Buffer.fill(swapSize)(Buffer.fill(pageSize)(0))
  val swapOwners:Buffer[Int] = Buffer.fill(swapSize)(-1)

  /**
   * Create tasks metadata
   */
  val io = new IOInterface(this)
  var tasks = scala.collection.mutable.Map[Int,Task]()
  val programResults = scala.collection.mutable.Map[Int,Int]()
  var semaphores = scala.collection.mutable.Map[Int,Int]() //semaphore id -> value
  
  /**
   * System booted, load it's shell and start scheduler
   */
  val shell = new Shell(this)
  val scheduler = new Scheduler(this)
  scheduler.start
  shell.start
  
  /*
   * Turns off this OS.
   */ 
  def shutDown:Unit = {
    this.isON = false
    scheduler ! "Stop"
  }

  /*
   * Returns the state for a given task id, returns -1 if the task does not exit (or has never existed)
   
   def getTaskState(taskId:Int):Int =
   if (tasksList.contains(taskId))
   tasksList(taskId).state
   else
   -1*/
  
  def getTask(taskId:Int):Task =
    if (tasks.contains(taskId))
      tasks(taskId)
    else
      throw invalidTaskIdentifier(taskId, ", unable to find: has never existed")
  /* 
   def echo(message:String){
   shell.print(message)
   }

def getBurstTime(anyUserProgram:programs.userProgram):Int = {
anyUserProgram.parsedLines.size
}*/

  def updateTaskInTasksList(taskId:Int,taskToUpdate:Task) = 
    if (!shell.systemPrograms.contains(taskToUpdate.name))
      tasks.update(taskId,taskToUpdate)

  def isRegister(name:String) = registers.contains(name.toUpperCase)

  def forkProcess(taskId:Int, command:List[String],output:outputMethod) = {
    val programMeta = shell.getTaskMeta(command.mkString(" "))
    programMeta match {
      case Some(taskMeta) => {
        val newTaskId = shell.exec(taskId,taskMeta,tasks.get(taskId).get.verbose)
        scheduler.tasksLog.info("taskId "+taskId.toString+" has fork a new child, taskId: "+tasks.get(newTaskId).get.toString)
        newTaskId
      }
      case _ => -1
    }
  }
  
  def getStateValue(taskId:Int):Int = {
    tasks.get(taskId).get.state
  }
  
  def getEndValue(taskId:Int):Int = {
    if (programResults.contains(taskId))
      programResults.get(taskId).get
    else{
      if (tasks.contains(taskId))
        0
    else
      -1
    }
  }
  
  /**
   * Funcionality
   */
  //def getOutput(isAsyncronous:Boolean, pathToFile:String) = 

  
  /**
   * Operations
   */
  def addRegisters(register1:String,register2:String):Int = try{
    getRegisterValue(register1) + getRegisterValue(register2)
  } catch {
    case _ : java.lang.NumberFormatException => throw new typeMismatchException("Could not add register "+register1+" to " +register2)
  }

  def substractRegisters(register1:String,register2:String):Int = try{
    getRegisterValue(register1) - getRegisterValue(register2)

  } catch {
    case _ : java.lang.NumberFormatException => throw new typeMismatchException("Could not substract register "+register1+" to " +register2)
  }

  def multiplyRegisters(register1:String,register2:String):Int = try{
    getRegisterValue(register1) * getRegisterValue(register2)
  } catch {
    case _ : java.lang.NumberFormatException => throw new typeMismatchException("Could not substract register "+register1+" to " +register2)
  }

  def divideRegisters(dividend:String,divisor:String):Int = try{
    getRegisterValue(dividend) / getRegisterValue(divisor)
  } catch {
    case _ : java.lang.NumberFormatException => throw new typeMismatchException("Could not divide register "+dividend+" to " +divisor)
  }

  def modRegisters(dividend:String,divisor:String):Int = try{
    getRegisterValue(dividend) % getRegisterValue(divisor)
  } catch {
    case _ : java.lang.NumberFormatException => throw new typeMismatchException("Could not divide register "+dividend+" to " +divisor)
  }

  def updateRegisterValue(register:String,value:Int) = if (registers.contains(register.toUpperCase)) {
    this.registers.update(register.toUpperCase,value)
  } else
    throw new registerNameNotFoundException(register)

  def getRegisterValue(register:String) = if (registers.contains(register.toUpperCase))
    registers.get(register.toUpperCase).get
                                          else
                                            throw new registerNameNotFoundException(register)

  /*
   * Checks for the real value for a given String,
   * e.g. R0, R1.. 1,2..
   */
  def getRealValue(value:String) = if (isRegister(value)) getRegisterValue(value) else value.toInt
} //end class OperatingSystem
