package core

import programs._
import java.io.File

object shell {
	var currentDir:String = ""

	val systemPrograms:List[String] = List("ls","ps","kill","debug","statsgen","top")
 
	def start(root_directory:String) = {
          currentDir = root_directory
          Console.println("""This is the ksh (kmels shell)
           __                          __           ___     ______
          [  |  _                     [  |        .'   `. .' ____ \
           | | / ]  _ .--..--.  .---.  | |  .--. /  .-.  \| (___ \_|
           | '' <  [ `.-. .-. |/ /__\\ | | ( (`\]| |   | | _.____`.
           | |`\ \  | | | | | || \__., | |  `'.'.\  `-'  /| \____) |
          [__|  \_][___||__||__]'.__.'[___][\__) )`.___.'  \______.'
          """);

          //get the available programs
          while(true){
            //ask for the entry to the user
            //entry = List.fromString(Console.readLine("ksh-1.0$> "),' ')
            val input:String = Console.readLine("ksh-1.0$> ")

            if (input.length!=0){
              //program, priority,memory and output object
              val programMetaToExecute:(program,Int,Int,outputMethod) = getProgramFromEntry(input)
              //parent, program to Execute, do verbose
              try{
                 exec(0,programMetaToExecute,false)
              } catch{
                case e => println(e.toString+"\n"+e.getStackTraceString)
              }
            }
          }
	}
  
	//def exec(parentTaskId:Int,entry:String,outputObject:outputMethod,doVerbose:Boolean):Int = {
        def exec(parentTaskId:Int,programMetaToExecute:(program,Int,Int,outputMethod),doVerbose:Boolean):Int = {
	  try {
            val programToExecute:programs.program = programMetaToExecute._1
            val priority:Int = programMetaToExecute._2
            val requiredFrames:Int = programMetaToExecute._3
            val outputObject:outputMethod = programMetaToExecute._4
            val burstTime = programToExecute.burstTime

            if (programToExecute!=null){
              if (systemPrograms.contains(programToExecute.programName)){
                //system programs have are not scheduled
                //don't add it to the queue, It's a system program, so execute it.'
                //priority, parent and id = 0, no verbose, no registers, just program to execute
                val newSystemTask = new task(0,0,programToExecute,scala.collection.mutable.Map(),0,0,false)
                newSystemTask.execNext
                -1
              }else{
                if (programToExecute!=null)
                  if ((!outputObject.outputIsAsynchronous) || (parentTaskId!=0))
                    //run in synchronous mode
                    sys.scheduler.runProgram(parentTaskId,programToExecute,priority,requiredFrames,doVerbose)
                  else{
                    //asynchronous mode, we've to return the new tasks' id
                    val newTask = sys.getNewTask(parentTaskId,programToExecute,priority,requiredFrames,doVerbose)
                    sys.scheduler ! newTask
                    newTask.id
                  }
                else
                        -1
              }
            } else
              -1
	  } catch{
	    case commandUnknown:exceptions.unkownCommandException => {println(commandUnknown.toString) ;-1} 
	    case wrongParametersException:exceptions.typeMismatchException => {println("ksh-1.0$> "+wrongParametersException.toString); -1}
            //case exception => {println("ksh-1.0$> "+exception.toString); -1}
	  }
   	}

        //returns program,priority,number of assigned pages
 	def getProgramFromEntry(input:String):(program,Int,Int,outputMethod) = {
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

          /*val programCallRE = """(.*)\s*((p\d+)?|(m\d+)?)\s*((m\d+)?)""".r

          //program call, priority, memory assigned
          val command:(String,Int,Int) = input match{
            case programCallRE(commandName,prioritySpec,hasPriority,memory,hasMemory) => {
                val p:Int = if (hasPriority==null)
                  4
                else
                  prioritySpec.substring(1).toInt


                val m:Int = if (hasMemory==null)
                  2 //default pages assigned
                else
                  memory.substring(1).toInt
                
                //yield command
                (commandName,p,m)
            }
            case _ => throw new Exception("invalid input")
          }*/

          //command name = command till the first ' ' is
          //val commandName = command._1.substring(0,command._1.indexOf(' ')+1)

          val asyncRE = """(.*)\s+?>\s+?(.+)""".r
          val syncRE = """(.*)""".r

          //commandName,parameters outputobject
          val commandSpec:(String,List[String],outputMethod) = command._1 match {
            case asyncRE(programCall,fileName) => {
                val entry = programCall.split(' ').toList
                (entry.head,entry.tail,sys.getOutput(true, currentDir+fileName))
            }
            case _ => {
                val entry = command._1.split(' ').toList
                (entry.head,entry.tail,sys.getOutput(false, ""))
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
              val programToExecute = commandName match{
                case "ls" => new ls(currentDir,output)
                case "ps" => new ps(output)
                case "kill" => {
                  if (parameters != 0){
                      val taskIdToKill = parameters(0).toInt
                      new kill(taskIdToKill,output)
                  } else
                      throw new exceptions.typeMismatchException("Parameters value must be int, found: "+parameters(0))
               }
                case "debug" => {
                    if (parameters.size > 0){
                      new debug(parameters,output)
                    }else
                     throw new exceptions.typeMismatchException("Parameters missing for debug")
                }
                case "statsgen" => {
                    if (output.outputIsAsynchronous)
                      new statsgen(output)
                    else
                     throw new exceptions.typeMismatchException("Parameters missing for statsgen.")
                }
                case "top" => new top(output)
              }
              val max_params = programToExecute.number_of_max_params
              (programToExecute,priority,npages,output)
            } else if (isUserProgram(currentDir+commandName)){
              setRegisterValues(parameters)
              val programToExecute = new userProgram(new File(currentDir+commandName),output)
              (programToExecute,priority,npages,output)
            } else{
              throw new exceptions.unkownCommandException(commandName)
           }
            // NOTA : escribir el registers.toString en el toString del task.
	  } catch{
	    case wrongParametersException:exceptions.typeMismatchException => { println("ksh-1.0$> "+wrongParametersException) ; null}
	  }
 	}
  
 	def setRegisterValues(parameters:List[String]){
          for (parameterIndex <- 0 until parameters.size){
            try {
              sys.registers.update("R"+parameterIndex.toString,parameters(parameterIndex).toInt)
            } catch{
              case wrongParameterException:java.lang.NumberFormatException => throw new exceptions.typeMismatchException("Parameters value must be int, found: "+parameters(parameterIndex))
            }
          }
 	}
  
	def print(string:String) = Console.print(string)

	def isUserProgram(pathToFile:String) = if ((new File (pathToFile)).exists()) true else false
 
	def println(string:String) = Console.println(string)
}
