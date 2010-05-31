package core
import programs._
import java.io.File
import exceptions.{unknownCommandException,typeMismatchException}
import util.FS._

class Shell(os:OperatingSystem) {
  val systemPrograms:List[String] = List("ls","ps","kill","debug","statsgen","top","pwd","mkdir","cd","cat","lsFCB","du","cp","rm")
  var currentPath:Path = new fsPath("~/")

  val absolutePathREGEX = """~/(.*)""".r
  val hostPathREGEX = """@/(.*)""".r
  val parentRelativePath = """../(.*)""".r
  /**
   * Returns an absolutePath:Path given a canonicalPath (relative to the currentDir)
   */ 
  def getAbsolutePathFromCanonical(canonicalPath:String):Path = canonicalPath match {
    case absolutePathREGEX(canonical) => new fsPath(appendSlash(canonicalPath))
    case hostPathREGEX(canonical) => new homePath(appendSlash(canonical))
    case ".." => currentPath match{
      case fsPath("~/") => currentPath
      case fsPath(pathToCurrent) => {
        val pathToCurrentComponents = pathToCurrent.split('/') 
        new fsPath(appendSlash(pathToCurrentComponents.slice(0,pathToCurrentComponents.size-1).mkString("/")))
      }
      case homePath(pathToCurrent) => {
        val pathToCurrentComponents = pathToCurrent.split('/') 
        new homePath(appendSlash(pathToCurrentComponents.slice(0,pathToCurrentComponents.size-1).mkString("/")))
      }
    }
    case parentRelativePath(relativePath) => currentPath match{
      case fsPath("~/") => new fsPath("~"+relativePath)
      case fsPath(pathToCurrent) => {
        val pathToCurrentComponents = pathToCurrent.split('/')
        val pathToParent = appendSlash(pathToCurrentComponents.slice(0,pathToCurrentComponents.size-1).mkString("/"))
        new fsPath(pathToParent+relativePath)
      }
      case homePath(pathToCurrent) => {
        val pathToCurrentComponents = pathToCurrent.split('/')
        val pathToParent = appendSlash(pathToCurrentComponents.slice(0,pathToCurrentComponents.size-1).mkString("/"))
        new homePath(pathToParent+relativePath)
      }
    }
    case _ => currentPath match {
      case fsPath(current) => new fsPath(current+appendSlash(canonicalPath))
      case homePath(current) => new homePath(current+appendSlash(canonicalPath))
    }
  }
    
  /**
   * Sets the shell current dir
   */
  def setCurrentPath(newPath:Path){ currentPath = newPath}

  /**
   * Starts the shell, asks for input, executes the program if it's a system program e.g. "ps" or "ls". If it's a user program, it sends the OS' scheduler the new instanced task.
   */
  def start = {
    Console.println("""This is the ksh (kmels shell)
                    __                          __           ___     ______
                    [  |  _                     [  |        .'   `. .' ____ \
                    | | / ]  _ .--..--.  .---.  | |  .--. /  .-.  \| (___ \_|
                    | '' <  [ `.-. .-. |/ /__\\ | | ( (`\]| |   | | _.____`.
                    | |`\ \  | | | | | || \__., | |  `'.'.\  `-'  /| \____) |
                    [__|  \_][___||__||__]'.__.'[___][\__) )`.___.'  \______.'
                    """);

    //get the available programs
    while(os.isON){
      //ask for users input
      val input:String = readLine("ksh-1.0$> ")
      if (input.length!=0){
        if (input=="exit"){
          println("Shutting down..")
          os.shutDown
        }
        else 
          try{
            //taskMeta: program, priority,memory and output object
            val possibleTaskMeta:Option[(userProgram,Int,Int,outputMethod)] = getTaskMeta(input)
            possibleTaskMeta match {
              case Some(taskMeta) => exec(0,taskMeta,false) //0 as parent
              case _ => {} // do nothing, system program was executed
            }
          } catch{
            case commandNotFound:unknownCommandException => println(commandNotFound.toString)
            case e => println(e.toString+"\n"+e.getStackTraceString)
          }
      }
    }
  }
  
  /**
   * Sends the task to the OS to schedule it.
   *
   * Returns the new tasks Id 
   */
  def exec(parentTaskId:Int,programMetaToExecute:(userProgram,Int,Int,outputMethod),doVerbose:Boolean):Int = {
    try {
      val programToExecute:userProgram = programMetaToExecute._1
      val priority:Int = programMetaToExecute._2
      val requiredFrames:Int = programMetaToExecute._3
      val outputObject:outputMethod = programMetaToExecute._4
      val burstTime = programToExecute.burstTime
      
      //If parentsId is not 0 i.e. it's a parent task forking or threading, it inherits the tasks output mechanism
      if ((!outputObject.outputIsAsynchronous) || (parentTaskId!=0))
        //run in synchronous mode
        os.scheduler.runProgram(parentTaskId,programToExecute,priority,requiredFrames,doVerbose)
      else{
        //asynchronous mode, we've to return the new tasks' id 
        val taskToExecute = os.scheduler.getNewTask(parentTaskId,programToExecute,priority,requiredFrames,doVerbose)       
        os.scheduler ! taskToExecute
        taskToExecute.id
      }
    } catch{
      case commandUnknown:unknownCommandException => {println(commandUnknown.toString) ;-1} 
      case wrongParametersException:typeMismatchException => {println("ksh-1.0$> "+wrongParametersException.toString); -1}
    }
  }

  /**
   * Parses an users input and returns an Option[tuple] Some(userProgram,priority,assigned-frames,outputMethod) where:
   * userProgram is an instance of the program the user is asking e.g. fibonacci
   * priority: the priority that the task will have
   * assigned-frames: assigned frames to the task
   * outputMethod: either if it's asynchrounous or synchrounous
   *
   * Returns Some() when the program is in fact an user program
   * Returns None if the program is a system program and was executed.
   */
  def getTaskMeta(input:String):Option[(userProgram,Int,Int,outputMethod)] = {
    val memoryRE = "m(\\d+)"
    val priorityRE = "p(\\d+)"

    val inputRE = "(.+?)\\s*(m(\\d+)|p(\\d+))?\\s*(m(\\d+)|p(\\d+))?".r
    val command:(String,Int,Int) = input match{
      case inputRE(commandInput,firstSpec,firstSpecMemory,firstSpecPriority,lastSpec,lastSpecMemory,lastSpecPriority) =>{
        val p:Int = if ((firstSpec==null) && (lastSpec==null))
          4
                    else
                      if (firstSpecPriority!=null)
                        firstSpecPriority.toInt
                      else
                        if (lastSpecPriority!=null)
                          lastSpecPriority.toInt
                        else
                          4

        val m:Int = if (firstSpec==null && lastSpec==null)
          2
                    else
                      if (firstSpecMemory!=null)
                        firstSpecMemory.toInt
                      else
                        if (lastSpecMemory!=null)
                          lastSpecMemory.toInt
                        else
                          2

        (commandInput,p,m)
      }
      case _ => throw new Exception("invalid input")
    }

    val asyncRE = """(.*)\s+?>\s+?(.+)""".r
    val syncRE = """(.*)""".r

    //commandName,parameters outputobject
    val commandSpec:(String,List[String],outputMethod) = command._1 match {
      case asyncRE(programCall,fileName) => {
        val entry = programCall.split(' ').toList
        val output = new outputMethod(os,true,os.pathToHome+fileName)
        (entry.head,entry.tail,output)
      }
      case _ => {
        val entry = command._1.split(' ').toList
        val output = new outputMethod(os,false,"")
        (entry.head,entry.tail,output)
      }
    }

    val commandName:String = commandSpec._1
    val parameters:List[String] = commandSpec._2
    val output:outputMethod = commandSpec._3
    
    val priority = command._2 //default priority
    val npages = command._3 //default number of pages to assign

    //check priority
    if ((priority > 9) || (priority < 0))
      throw new exceptions.typeMismatchException("Priority value must be higher than 0 and less than 10")
    
    try {
      //try if it's an executable command
      if (systemPrograms.contains(commandName)){
        executeSystemProgram(commandName,parameters,output)
        None
      } else if (isUserProgram(currentPath+commandName)){
        setRegisterValues(parameters)
        val programToExecute = new userProgram(new File(currentPath+commandName),output)
        Some(programToExecute,priority,npages,output)
      } else{
        throw new exceptions.unknownCommandException(commandName)
      }
      // NOTA : escribir el registers.toString en el toString del task.
    } catch{
      case wrongParametersException:exceptions.typeMismatchException => { println("ksh-1.0$> "+wrongParametersException) ; null}
    }
  } // end getTaskMeta
  
  def setRegisterValues(parameters:List[String]){
    for (parameterIndex <- 0 until parameters.size){
      try {
        os.registers.update("R"+parameterIndex.toString,parameters(parameterIndex).toInt)
      } catch{
        case wrongParameterException:java.lang.NumberFormatException => throw new exceptions.typeMismatchException("Parameters value must be int, found: "+parameters(parameterIndex))
      }
    }
  }
  
  /**
   * Executes a system program e.g. "ps" or "ls"
   */
  def executeSystemProgram(programName:String,parameters:List[String],output:outputMethod):Unit = {   
    //instance the program
    val systemProgram:system_program = programName match{
      case "ls" => parameters.size match {
        case 0 => new ls(os,currentPath,output) //list current path
        case 1 => new ls(os,getAbsolutePathFromCanonical(parameters(0)),output)
        case _ => throw new exceptions.typeMismatchException("ls receveis one parameter at most, received: "+parameters.mkString(","))
      }
      case "ps" => new ps(os,output)
      case "kill" => {
        if (parameters != 0){
          val taskIdToKill = parameters(0).toInt
          new kill(os,taskIdToKill,output)
        } else
          throw new exceptions.typeMismatchException("Parameters value must be int, found: "+parameters(0))
      }
      case "debug" => {
        if (parameters.size > 0){
          new debug(os,parameters,output)
        }else
          throw new exceptions.typeMismatchException("Parameters missing for debug")
      }
      case "statsgen" => {
        if (output.outputIsAsynchronous)
          new statsgen(os,output)
        else
          throw new exceptions.typeMismatchException("Parameters missing for statsgen.")
      }
      case "top" => new top(os,output)
      case "pwd" => new pwd(currentPath,output)
      case "mkdir" => 
        if (parameters.size==1)
          new mkdir(os,getAbsolutePathFromCanonical(parameters(0)),output)
        else
          throw new exceptions.typeMismatchException("Mkdir accepts one parameter only")
      case "lsFCB" => new lsFCB(os,output)
      case "cd" => {
        if (parameters.size==1)
          new cd(os,getAbsolutePathFromCanonical(parameters(0)),output)
        else
          throw new exceptions.typeMismatchException("cd accepts one parameter only")
      }
      case "du" => parameters.size match {
        case 0 => new du(os,currentPath,output)
        case 1 => new du(os,getAbsolutePathFromCanonical(parameters(0)),output)
        case _ => throw new exceptions.typeMismatchException("du accepts no parameters")
      }
      case "cat" =>{
        if (parameters.size==1)
          new cat(os,getAbsolutePathFromCanonical(parameters(0)),output)
        else
          throw new exceptions.typeMismatchException("cd accepts one parameter only")
      }
      case "cp" => {
        if (parameters.size==2){
          val sourcePath:Path = getAbsolutePathFromCanonical(parameters.head)
          val destinyPath:Path = getAbsolutePathFromCanonical(parameters.last)
          new cp(os,sourcePath,destinyPath,output)
        }else{
          throw new exceptions.typeMismatchException("cp accepts two parameters")
        }
      }
      case "rm" => {
        if (parameters.size==1)
          new rm(os,getAbsolutePathFromCanonical(parameters(0)),output)
        else
          throw new exceptions.typeMismatchException("rm accepts one parameter only")
      }
    }
    val max_params = systemProgram.number_of_max_params
    //execute it 
    systemProgram.exec
  }
  def print(string:String) = Console.print(string)

  def isUserProgram(pathToFile:String) = if ((new File (pathToFile)).exists()) true else false
  
  def println(string:String) = Console.println(string)
}
