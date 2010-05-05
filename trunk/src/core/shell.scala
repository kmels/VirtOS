package core

import programs._
import java.io.File

object shell {
	var currentDir:String = ""

	val systemPrograms:List[String] = List("ls","ps","kill","debug","statsgen")
 
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
          var entry:List[String] = Nil
          while(true){
            //ask for the entry to the user
            entry = List.fromString(Console.readLine("ksh-1.0$> "),' ')

            if (entry.length!=0){
              exec(0,entry,getOutputObject(entry),false)
            }
          }
	}
  
	def exec(parentTaskId:Int,entry:List[String],outputObject:outputMethod,doVerbose:Boolean):Int = {
	  try {
            val programFromEntry = getProgramFromEntry(entry,outputObject)
            val programToExecute:programs.program = programFromEntry._1
            val priority = programFromEntry._2
            val burstTime = programToExecute.burstTime

            if (programToExecute!=null){
              if (systemPrograms.contains(programToExecute.programName)){
                //system programs have are not scheduled
                //don't add it to the queue, It's a system program, so execute it.'
                //priority, parent and id = 0, no verbose, no registers, just program to execute
                val newSystemTask = new task(0,0,programToExecute,scala.collection.mutable.Map(),0,false)
                newSystemTask.execNext
                -1
              }else{
                if (programToExecute!=null)
                  if ((!outputObject.outputIsAsynchronous) || (parentTaskId!=0))
                    //run in synchronous mode
                    sys.scheduler.runProgram(parentTaskId,programToExecute,priority,doVerbose)
                  else{
                    //asynchronous mode, we've to return the new tasks' id
                    val newTask = sys.getNewTask(parentTaskId,programToExecute,priority,doVerbose)
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
	    case wrongParametersException:exceptions.typeMismatchException => {println("ksh-1.0$> "+wrongParametersException); -1}
            //case exception => {println("ksh-1.0$> "+exception.toString); -1}
	  }
   	}

        def getOutputObject(entry:List[String]) = {
 	  val commandName = entry.head
	  var parameters = entry.tail

	  //check the output method
	  var isAsynchronous = if (parameters.contains(">")) true else false
	  var fileName = ""

	  //adjust parameters and set fileName
	  if (isAsynchronous){
	      fileName = parameters(parameters.indexOf(">") + 1)
	      parameters = parameters.slice(0,parameters.indexOf(">"))
	      sys.getOutput(isAsynchronous,currentDir+fileName)
	  } else{
		  sys.getOutput(isAsynchronous,currentDir+fileName)
	  }
 	}

        //returns program,priority
 	def getProgramFromEntry(entry:List[String],outputObject:outputMethod):(program,Int) = {
 	  val commandName = entry.head
	  var parameters = entry.tail
				
	  //adjust parameters and set fileName
	  val output = outputObject
          
	  var priority = 4 //default priority

          //check if it has priority specified
          if ((parameters.size>0) && (parameters.last.startsWith("p"))){

            if (parameters.last.size > 1)
              priority = parameters.last.drop(1).toInt
            else
              throw new exceptions.typeMismatchException("Please specify the priority value")

            parameters = parameters.slice(0, parameters.size-1)
            
            if ((priority > 9) || (priority < 0))
              throw new exceptions.typeMismatchException("Priority value must be higher than 0 and less than 10")
          }

	  if (output.outputIsAsynchronous){
	    parameters  = parameters.slice(0,parameters.indexOf(">"))
	  }

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
                      new debug(parameters,outputObject)
                    }else
                     throw new exceptions.typeMismatchException("Parameters missing for debug")
                }
                case "statsgen" => {
                    if (outputObject.outputIsAsynchronous)
                      new statsgen(outputObject)
                    else
                     throw new exceptions.typeMismatchException("Parameters missing for statsgen.")
                }
              }
              val max_params = programToExecute.number_of_max_params
              (programToExecute,priority)
            } else if (isUserProgram(currentDir+commandName)){
              setRegisterValues(parameters)
              val programToExecute = new userProgram(new File(currentDir+commandName),output)
              (programToExecute,priority)
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
