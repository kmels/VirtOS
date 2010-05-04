package core

import java.io.File
import programs.userProgram
import exceptions._

class task(parent:Int, pid:Int,programToExecute:programs.program,registerValues:scala.collection.mutable.Map[String,Int],priorityOfTask:Int,doVerbose:Boolean) {
    val verbose = doVerbose
    val id = pid
    val parentId = parent    
    val burstTime:Int = programToExecute.burstTime
    val priority:Int = priorityOfTask
    var state : Int = 1
    val name = programToExecute.programName
    var registers = scala.collection.mutable.Map[String,Int]() 
    var waitingTime = 0
    var waitingTimeSinceLastIO = 0
    var timeInCPU = 0
    var tickWhenStartedExecuting = -1
    var tickWhenCreated = 0

    def responseTime:Int = tickWhenStartedExecuting - tickWhenCreated

    saveRegisters(registerValues)
    
    setStateTo("New")
    
    val userProgramToExecute:userProgram =
      programToExecute match{
        case anyUserProgram:userProgram => anyUserProgram
        case _ => null
      }
    
    val output = programToExecute.output
    
    //simulate the memory instructions
	var executionHasFinished = false
 
    var contextSwitchFlag = false
    
    def saveRegisters(registersToSave:scala.collection.mutable.Map[String,Int]){
      registersToSave foreach {
        register => this.registers.update(register._1,register._2)
      }
    }
    /**
    * Task stuff
    */
    
    override def toString = "pid: "+id.toString + ", programName: "+name+", priority: "+priority.toInt.toString+", CPU burst: "+burstTime.toInt.toString + ", parent_id: "+parentId.toString +", state_id: "+state.toString +", waitingTimeSinceLastIO: "+waitingTimeSinceLastIO.toString//+ " registers: "+registers.toString
    
    def setStateTo(value:Int){
      sys.updateTaskInTasksList(this.id,this)
      state = value
    }
    
    def setStateTo(state:String) {
      state.toUpperCase match {
        case "NEW" => setStateTo(1)
        case "READY" => setStateTo(2)
        case "RUNNING" => setStateTo(3)
        case "WAITING" => setStateTo(4)
        case "TERMINATED" => setStateTo(5)
        case unknownStateId => throw new unknownStateIdException(unknownStateId)
      }
      sys.tasksLog.info("Changed task: "+this.toString + " to state \""+state+"\"")
    }
    
    def execAndFinish(aSystemProgram:programs.system_program) = {
      aSystemProgram.exec
      this.executionHasFinished = true
    }
    
    def execNext() = programToExecute match{
      case anyUserProgram:userProgram => userProgramInterpreter.executeNextLine
      case anySystemProgram:programs.system_program => execAndFinish(anySystemProgram)
    }
    
    /**
     * End task stuff
     */
     
    object userProgramInterpreter {
    	val filteredLines:List[(Int,List[String])] = userProgramToExecute.parsedLines
	val labels:scala.collection.mutable.Map[String,Int] = userProgramToExecute.labels	    
	var didFork = false
        var waitForSemaphore = false
        var foundLabel = false
	    /**
	     * For execution purposes
	     */
	    
        def printIfVerbose(message:String) = if (verbose) output.echo("*** [Debug,tick "+sys.scheduler.ticks.size.toString+"] -  "+message+"\n")
          printIfVerbose("Executing task: pid: "+id.toString + " programName: "+name+" parent_id: "+parentId +" state_id: "+state)
          printIfVerbose("Labels: ")
          for (label <- labels){
            printIfVerbose(label._1+" in line "+label._2)
          }

      //define the index of the line in the list of filteredLines to execute
      var indexOfExecutingLine =
              if (filteredLines.size>0)
                      0
              else
                      -1

      //if there's no more than 0 instructions, don't execute
      if (indexOfExecutingLine== -1)
              executionHasFinished = true

      //the next line to execute should be, the next one (if not jumping)
      def getNextExecutingLineIndex = if(indexOfExecutingLine+1 < filteredLines.size)
               indexOfExecutingLine+1
      else
              -1

      //get the index of the line number given
      def getIndexOfLine(lineNumber:Int) = {
               val lineWeWant = filteredLines.filter(_._1 == lineNumber)(0)
               filteredLines.indexOf(lineWeWant)
      }

      def get_line_value_from_line_number(lineNumber:Int) = filteredLines.filter(_._1 == lineNumber)(0)._2
      def getLabelName(label:String) = label.substring(1,label.length-1).toUpperCase

      def executeNextLine() = {
            didFork = false
            waitForSemaphore = false
            contextSwitchFlag = false
            foundLabel = false
        //execute the next line

        val lineNumber = filteredLines(indexOfExecutingLine)._1
        val lineValue =  filteredLines(indexOfExecutingLine)._2
        executeInstruction(lineNumber,lineValue)
      }

      def getNextInstructionMeta():(Int,String) = {
        var nextInstruction = ""

        var lineNumber = filteredLines(indexOfExecutingLine)._1
        var lineValue =  filteredLines(indexOfExecutingLine)._2

        //check if it's label
        List(lineValue.head.toUpperCase):::lineValue.tail match{
          case List(possibleLabel:String) => {
              if (labels.contains(getLabelName(possibleLabel))){
               //it's labe, return the next instruction
                lineNumber = filteredLines(indexOfExecutingLine+1)._1
                lineValue = filteredLines(indexOfExecutingLine+1)._2
              }
          }
          case _ => {}
        }

        lineValue.foreach(token =>{
          nextInstruction += token + " "
        })

        (lineNumber,nextInstruction)
      }

      def executeInstruction(lineNumber:Int,lineValue:List[String]){
        printIfVerbose("Program: "+name+" ,id: "+id.toString+", line:"+ lineNumber.toString+" ,value: " +lineValue.toString)
          try {
            var hasToJump = false
            didFork = false
            waitForSemaphore = false
            var fileIsInUseByAnotherTask = false
            List(lineValue.head.toUpperCase):::lineValue.tail match{

            //Set reg value
              case List("SET",register:String,value:String) => {
                if (sys.isRegister(value)){
                        sys.updateRegisterValue(register,sys.getRegisterValue(value))
                        printIfVerbose("Updated register "+register+" to value "+sys.getRegisterValue(value))
                }
                else {
                        sys.updateRegisterValue(register,value.toInt)
                        printIfVerbose("Updated register "+register+" to value "+value)
                }
              }

              case List("ADD",register_to_update,register_operator1,register_operator2) => {
                val valueForAdd = sys.addRegisters(register_operator1,register_operator2)
                sys.updateRegisterValue(register_to_update,valueForAdd)
                printIfVerbose("Updated register "+register_to_update+" to value "+valueForAdd)
              }

              case List("SUB",register_to_update,register_operator1,register_operator2) => {
                val valueForSub = sys.substractRegisters(register_operator1,register_operator2)
                sys.updateRegisterValue(register_to_update,valueForSub)
                printIfVerbose("Updated register "+register_to_update+" to value "+valueForSub)
              }

              case List("MUL",register_to_update,register_operator1,register_operator2) => {
                val valueForMul = sys.multiplyRegisters(register_operator1,register_operator2)
                sys.updateRegisterValue(register_to_update,valueForMul)
                printIfVerbose("Updated register "+register_to_update+" to value "+valueForMul)
              }

              case List("DIV",register_to_update_with_quotient,register_dividend,register_divisor,register_to_update_with_remainder) => {
                      val valueDiv = sys.divideRegisters(register_dividend,register_divisor)
                      sys.updateRegisterValue(register_to_update_with_quotient,valueDiv)
                      val valueRemainder = sys.modRegisters(register_dividend,register_divisor)
                      sys.updateRegisterValue(register_to_update_with_remainder,valueRemainder)
              }

              case List("JMP",register_to_check,labelToJump) =>
                    printIfVerbose("found Instruction: JMP "+register_to_check+" "+labelToJump)
                    printIfVerbose("Checking "+register_to_check+"'s value")

                    if (sys.getRegisterValue(register_to_check)>0){
                     try {
                       hasToJump = true
                       val lineNumberWhereLabelIs = labels.get(labelToJump.toUpperCase).get
                       indexOfExecutingLine = getIndexOfLine(lineNumberWhereLabelIs)
                       printIfVerbose("Jumping to label "+labelToJump.toString+" line of program: "+lineNumberWhereLabelIs.toString)
                     } catch{
                       case _ => throw new invalidLabelException(labelToJump,lineNumber)
                     }
                    } else{
                       printIfVerbose("Not jumping, register "+register_to_check+" has value "+sys.getRegisterValue(register_to_check).toString)
                    }

              case List("ECHO",message) => {
                  output.echo(message)
                  contextSwitchFlag = true
              }

              case List("END",endValue) => {
                  sys.scheduler.endTask(sys.getRealValue(endValue))
              }

              case List(simple_string:String) =>
                    simple_string match {
                      case "SLEEP" => {
                            printIfVerbose("found Instruction: SLEEP")
                        contextSwitchFlag = true
                      }
                      case another => {
                          if (!labels.contains(getLabelName(another)))
                            throw new executionException("could not understand command")
                          else{
                            printIfVerbose("found label: "+getLabelName(another)+", executing next instruction")
                            foundLabel = true
                          }
                            
                      }
                    }

              case List("OPENFILE",fileName:String,handlerRegister) => {
                  printIfVerbose("found Instruction: OPENFILE \""+fileName+"\" "+handlerRegister)
                  fileIsInUseByAnotherTask = sys.isFileInUseByAnotherTask(fileName,id)

                  contextSwitchFlag = true

                  if (!fileIsInUseByAnotherTask){
                    val handler = sys.openFile(id,fileName)
                    printIfVerbose("Opened file successfuly, handler id given: "+handler.toString)
                    sys.updateRegisterValue(handlerRegister,handler.toInt)
                    printIfVerbose("Updated register "+handlerRegister+" to value "+handler.toString)
                  } else
                    printIfVerbose("file is in use, doing nothing")
              }

              case List("CLOSEFILE",handlerValue:String) => {
                  val handlerId = sys.getRealValue(handlerValue)
                  printIfVerbose("found Instruction: CLOSEFILE "+handlerId.toString)

                  contextSwitchFlag = true

                  sys.closeFile(id,handlerId)
              }

              case List("READLINE",handlerValue,register2) => {
                  val handlerId:Int = sys.getRealValue(handlerValue)

                  printIfVerbose("found Instruction: READLINE "+handlerId.toString+" "+register2)

                  //check if the handlerId is in fact opened.
                  val fileName = sys.fileNames.get(handlerId).get

                  printIfVerbose("trying to read file named "+fileName)
                  fileIsInUseByAnotherTask = sys.isFileInUseByAnotherTask(fileName,id)

                  contextSwitchFlag = true

                  if (!fileIsInUseByAnotherTask){
                    val lineRead = sys.readLineFromFile(id,handlerId.toInt)
                    val length = if (lineRead != null){
                      output.echo(lineRead+"\n")
                      lineRead.length+4 //because of the /r /n
                    } else
                      0

                    sys.updateRegisterValue(register2,length)
                    printIfVerbose("Updated register "+register2+" to value "+length.toString)
                  }
              }

              case List("WAIT",semaphore) => {
                val semaphoreId = sys.getRealValue(semaphore)
                if (!sys.semaphores.contains(semaphoreId)){
                    //create it
                sys.semaphores.put(semaphoreId, 0)
                }
                else
                  if (sys.semaphores(semaphoreId)==0){
                    //wait
                    waitForSemaphore = true
                  }
              }

              case List("SIGNAL",semaphore) => {
                val semaphoreId = sys.getRealValue(semaphore)
                sys.semaphores.put(semaphoreId,1)
              }

              case List("FORK",command,register) => {
                didFork = true
                
                printIfVerbose("found Instruction:\"FORK "+command+" "+register+"\"")
                val childId = sys.forkProcess(id,command.split(' ').toList,output)

                printIfVerbose("fork was successful, child id given:"+childId.toString)
                sys.updateRegisterValue(register,childId)
                printIfVerbose("Updated register "+register+" to value "+childId.toString)
              }

              case List("GETSTATE",taskIdValue,register2,register1) => {
                println("pidio getstate")
                val taskId = sys.getRealValue(taskIdValue)
                val stateValue = sys.getStateValue(taskId)
                val endValue = sys.getEndValue(taskId)

                printIfVerbose("found Instruction: GETSTATE "+taskId.toString+" "+register2+" "+register1)

                sys.updateRegisterValue(register2,stateValue)
                sys.updateRegisterValue(register1,endValue)

                printIfVerbose("State of taskId "+taskId.toString+": "+stateValue.toString+", end value: "+endValue.toString)
              }

              case List("RANDOM",register1,register2,register3) => {
                val atLeast = sys.getRegisterValue(register1)
                val atMost = sys.getRegisterValue(register2)

                printIfVerbose("found Instruction: RANDOM "+atLeast.toString+" "+atMost.toString+" "+register3)

                //nextInt returns a pseudorandom number between 0 and the parameter (exclusive, therefore the +1), which is the amplitude
                val randomNumber = scala.util.Random.nextInt(atMost-atLeast+1) + atLeast
                sys.updateRegisterValue(register3,randomNumber)
              }

              case List(_*) => throw new executionException("could not understand command")
            } //end del match

            if ((!hasToJump) && (!fileIsInUseByAnotherTask) && (!waitForSemaphore)){
                printIfVerbose("Increased instruction pointer")
                indexOfExecutingLine = getNextExecutingLineIndex
            }

            if (contextSwitchFlag)
               printIfVerbose("Context switch")

            if (indexOfExecutingLine == -1)
              executionHasFinished = true

            //if this was a label, then execute next immediately
            if (foundLabel)
              this.executeNextLine
            
              
            
          } catch {
            case unkownException : exceptions.executionException  => throw new executionException("Task: "+name+"Execution error at line "+lineNumber.toString+": "+unkownException.toString)
            case knownException => throw new executionException("Execution error at line "+lineNumber.toString+": "+knownException.toString)
          }
      } //end de execute instruction
      
    } 
}
