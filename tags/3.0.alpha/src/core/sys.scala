package core {
  	
  import programs.program
  import scala.collection.immutable.{ListSet}
  import scala.collection.mutable.{Queue,ArrayBuffer,Map,Buffer}
  import exceptions._
  import util.{schedulingQueue,tickMeta}

  /**
   * NOTA: Arreglar queue
   */
  object sys {
    var rootDirectory:String = ""

    //Logging
    val tasksLog = new util.Log(rootDirectory+"logs/ps.log")
    tasksLog.info("Tasks log initialized")

    val bootLog = new util.Log(rootDirectory+"logs/boot.log")
    bootLog.info("Booting log initialized")

    val ioLog = new util.Log(rootDirectory+"logs/io.log")
    ioLog.info("I/O log initialized")

    val memLog = new util.Log(rootDirectory+"logs/mem.log")
    memLog.info("Mem log initialized")

    var tasksCounter = 0
    val job_queue = new Queue[task] 	//consists of all processes in the system
    val IOqueues = scala.collection.mutable.Map[String,Queue[Int]]()
    val programResults = scala.collection.mutable.Map[Int,Int]()
    var current = -1;
    var tasksList = scala.collection.mutable.Map[Int,task]()
    var queuesWhereTasksAre = scala.collection.mutable.Map[Int,schedulingQueue]() //task id, scheduling queue
    var files = scala.collection.mutable.Map[Int,java.io.BufferedReader]() //file handler id -> (taskId of the owner,FileHandler object)
    var fileOwners = scala.collection.mutable.Map[Int,Int]() //file handler id -> task owner id
    var fileNames = scala.collection.mutable.Map[Int,String]() //file handler id -> fileName
    var semaphores = scala.collection.mutable.Map[Int,Int]() //semaphore id -> value

    //dummy variables
    var ramSize:Int = -1
    var pageSize:Int = -1
    var swapSize:Int = -1

    object Config{
      def load(fileName:String) = {
          bootLog.info("Loading configuration file..")
          //load configuration file sys.xml
          val xmlConfiguration:scala.xml.Elem = scala.xml.XML.load(new java.io.FileInputStream(new java.io.File(rootDirectory+fileName)))

          //search for memory config
          val memoryXMLElement:scala.xml.NodeSeq = xmlConfiguration \\ "memory"
          //update dummy variables
          ramSize = (memoryXMLElement \ "@ram").text.toInt
          swapSize = (memoryXMLElement \ "@swap").text.toInt
          pageSize = (memoryXMLElement \ "@page").text.toInt
          
          //search for the scheduling element
          val scheduling:scala.xml.NodeSeq = xmlConfiguration \\ "scheduling"
          sys.scheduler.startQueue = (scheduling \ "@startQueue").text.toInt
          bootLog.info("Scheduling has startQueue id: "+sys.scheduler.startQueue.toString)

          val queuesInXML = scheduling \\ "queue"
          val nQueues:Int = queuesInXML.size

          bootLog.info("Found "+nQueues.toString+" scheduling queues in configuration file:")

          //iterate queues in XML
          for (schedulingQueue <- queuesInXML){
            val id:Int = (schedulingQueue \ "@id").text.toInt
            val name = (schedulingQueue \ "@name").text
            val typeOfQueue = (schedulingQueue \ "@type").text
            val quantum:Int = (schedulingQueue \ "@quantum").text.toInt
            val preemptive:Boolean = (schedulingQueue \ "@preemptive").text match{
              case "0" => false
              case _ => true
            }

            val queueOnQuantumEnd:Int = (schedulingQueue \ "@queueOnQuantumEnd").text.toInt
            val queueOnIOEnd:Int = (schedulingQueue \ "@queueOnIOEnd").text.toInt
            bootLog.info("id: "+id.toString+" name: "+name+" type: "+typeOfQueue+" quantum: "+quantum.toString+" preemptive: "+preemptive.toString+" queueOnQuantumEnd "+queueOnQuantumEnd+" queueOnIOEnd: "+queueOnIOEnd)

            val newQueue = new schedulingQueue(id,name,typeOfQueue,quantum,preemptive,queueOnQuantumEnd,queueOnIOEnd)
            sys.scheduler.queues += newQueue
          }
      }
    }

    Config.load("sys.xml")
    
    def init(root_dir:String){
      Console.println("Initializing.. rootdir:"+root_dir)
      shell.start(root_dir)
      rootDirectory = root_dir
    }
    
    val pageOwners:Buffer[Int] = Buffer.fill(ramSize)(-1)
    val memory:Buffer[Buffer[Int]] = Buffer.fill(ramSize)(Buffer.fill(pageSize)(0))
    val swap:Buffer[Buffer[Int]] = Buffer.fill(swapSize)(Buffer.fill(pageSize)(0))
    val swapOwners:Buffer[Int] = Buffer.fill(swapSize)(-1)

    scheduler.start()

    bootLog.info("Multilevel-feedback-queue configuration: "+scheduler.queues.toString)

    import scala.actors._
    import scala.actors.Actor._

    def getNewProcessId:Int = tasksCounter

    def getFramesForNewTask(howMany:Int,taskId:Int,parentId:Int):List[Int] = {
      memLog.info("task "+taskId.toString+" is asking for "+howMany.toString+" frames")

      val assignedFrames:List[Int] =
        if (parentId!=0){
          memLog.info("task's parent found: "+parentId.toString+", sharing frames")
          tasksList(parentId).frames.map(_._2).toList
        } else{
          val emptyPages = pageOwners.zipWithIndex.filter(_._1<0)
          memLog.info(emptyPages.size+" free frames were found: "+emptyPages.map(_._2).toList.mkString(","))
          if (emptyPages.size>=howMany){
            memLog.info("assigning free frames")
            val emptyFramesAssigned:List[Int] = emptyPages.slice(0, howMany).map(assignedPage => assignedPage._2).toList
            emptyFramesAssigned
          } else{
            memLog.info("trying to swap in/out")
            val swapUsed = swapOwners.find(_ > (-1)).size
            memLog.info("swap used: "+swapUsed.toString)
            val freeSwap = swapSize - swapUsed
            val freeSwapFramesIndexes = swapOwners.zipWithIndex.filter(_._1 < 0).map(_._2)
            memLog.info("free swap frames: "+freeSwapFramesIndexes.mkString(","))

            val pagesNotOwnedByCurrentTask:List[Int] = pageOwners.zipWithIndex.filter(_._1!=taskId).map(frame => frame._2).toList
            val swapedOutFrames = pagesNotOwnedByCurrentTask.slice(0,howMany)
            memLog.info("frames not owned by task asking for frames: "+pagesNotOwnedByCurrentTask.mkString(","))

            if (swapUsed < swapSize && freeSwap >= swapedOutFrames.size) {
              //swap out some
              memLog.info("swaping in and out frames")            
              memLog.info("swaping out frames from memory: "+swapedOutFrames.mkString(","))
              //falta poner el taskid.frames mapeadas a swap
              swapedOutFrames.foreach(swapedOutFrame => {
                  //find first which is free
                  val freeSwapFrames = swapOwners.zipWithIndex.filter(_._1 < 0)
                  val freeSwapFrame = freeSwapFrames.head
                  val indexToUpdate = freeSwapFrame._2
                  swapOwners(indexToUpdate) = pageOwners(swapedOutFrame)
                  memLog.info("swaping in swap frame "+indexToUpdate)
                  swap(indexToUpdate) = memory(swapedOutFrame)
              })

              swapedOutFrames
            } else{
              memLog.info("not enough swap space")
              //return empty list
              List()
            }
          }
        }

       memLog.info("assigned frames: "+assignedFrames.mkString(","))
       assignedFrames.foreach(assignedPage => pageOwners(assignedPage) = taskId)
       memLog.info("frame owners table updated")
       memLog.info("memory owners: "+pageOwners.mkString(","))
       memLog.info("swap owners: "+swapOwners.mkString(","))
       assignedFrames
    }

    def getNewTask(parentTaskId:Int,programToExecute:programs.program,priority:Int,pagesNeeded:Int,doVerbose:Boolean) = {
      tasksCounter += 1
      new task(parentTaskId,getNewProcessId,programToExecute,registers,priority,pagesNeeded,doVerbose)
    }

    /*
     * Returns the state for a given task id, returns -1 if the task does not exit (or has never existed)
     */
    def getTaskState(taskId:Int):Int =
      if (tasksList.contains(taskId))
        tasksList(taskId).state
      else
        -1

    def getTask(taskId:Int):task =
      if (tasksList.contains(taskId))
        tasksList(taskId)
      else
        throw invalidTaskIdentifier(taskId, ", unable to find: has never existed")

    object scheduler extends Actor{
      var activeSchedulingQueue:schedulingQueue = null
      var queueToPlaceTaskAfterIOWait = scala.collection.mutable.Map[Int,schedulingQueue]() //id,queueonQuantumEnd
      var tasksToKill:ListSet[Int] = new ListSet()
      var startQueue:Int = -1
      var currentTask:task = null
      var queues:ArrayBuffer[schedulingQueue] = new ArrayBuffer[schedulingQueue](20)
      var ticks = Map[Int,tickMeta]()
      var timeOfTaskInCPU:Int = 0
      var waitingQueue:Queue[task] = new Queue()
      var totalTimeOfWaitingQueue=0

      def queueToSchedule:schedulingQueue ={
        val startingSchedulingQueue = queues.find(_.getId==startQueue).get

        if (!startingSchedulingQueue.isEmpty){
          //if the start queue is not empty, we've to execute its tasks'
           startingSchedulingQueue
        }
        else{
            val nonEmptyQueues = queues.dropWhile(_.getSize==0)
            if (nonEmptyQueues.size>0){
              //return the first one that matches
              nonEmptyQueues.head
            }
            else{
              //if every queue is empty, return the start queue
              startingSchedulingQueue              
            }
        }
      }

      def updateAndCheckForWaitingQueue = {
        //increase the tick and the waiting time in waiting queue
        waitingQueue.foreach(waitingTask => {
            waitingTask.waitingTimeSinceLastIO += 1
            waitingTask.waitingTime += 1
        })

        if (waitingQueue.size>0)
          totalTimeOfWaitingQueue +=1

        val tasksToWakeUp = waitingQueue.dequeueAll(_.waitingTimeSinceLastIO == 5)

        if (tasksToWakeUp.size > 0){
          tasksToWakeUp.foreach(taskToWake =>{
            tasksLog("Waking up task: "+taskToWake.toString)
            taskToWake.setStateTo("ready")
            taskToWake.waitingTimeSinceLastIO = 0
            val queueWhereEnqueueing:schedulingQueue = queueToPlaceTaskAfterIOWait.get(taskToWake.id).get
            tasksLog("Adding task "+taskToWake.toString+" to "+queueWhereEnqueueing.getName)            
            queueWhereEnqueueing.enQueue(taskToWake)
            queuesWhereTasksAre.put(taskToWake.id,queueWhereEnqueueing)
            tasksLog(queueWhereEnqueueing.getName+": "+queueWhereEnqueueing.toString)
          })

          tasksLog("Waiting queue: "+waitingQueue.toString)
        }
      }

      def checkForTasksToKill = {
        if (!tasksToKill.isEmpty){
            sys.tasksLog("Found something in tasks to kill list: "+tasksToKill.toString)
            //check if the current process is meant to be killed
            if (tasksToKill.contains(currentTask.id)){
              currentTask.executionHasFinished = true
              //remove from the tasks to kill
              tasksToKill -= currentTask.id
              sys.tasksLog("Killing current task with value -1")

              //free ram
              val framesOwnedByTaskToKill = pageOwners.zipWithIndex.filter(_._1==currentTask.id).map(_._2).toList
              memLog.info("killing task "+currentTask.id+", frames owned: "+framesOwnedByTaskToKill.mkString(","))

              framesOwnedByTaskToKill.foreach(frameIndex => pageOwners(frameIndex) = -1 )

              //remove from the queue (kill it)
              val deQueuedTask = activeSchedulingQueue.deQueue

              currentTask.setStateTo("Terminated")
              //flush the tasks' output
              currentTask.output.flush

              tasksLog.info("Killed task: "+currentTask+" with value -1")
              programResults.put(currentTask.id,-1)
              //return -1
            }else{
             //something to kill in the ready queue
             tasksToKill foreach(taskToKillId => {
                 sys.tasksLog("Killing task id "+taskToKillId+" with value -1")

                //free ram
                val framesOwnedByTaskToKill = pageOwners.zipWithIndex.filter(_._1==taskToKillId).map(_._2).toList
                memLog.info("killing task "+taskToKillId+", frames owned: "+framesOwnedByTaskToKill.mkString(","))

                framesOwnedByTaskToKill.foreach(frameIndex => pageOwners(frameIndex) = -1 )

                 val queueWhereTaskIs = queuesWhereTasksAre.get(taskToKillId).get

                  val killedTask:task =
                    if (queueWhereTaskIs.Queue.exists(_.id == taskToKillId))
                    //then it's ready
                    queueWhereTaskIs.deQueue
                    else
                      waitingQueue.dequeueFirst(_.id == taskToKillId).get

                 killedTask.setStateTo("Terminated")
                 killedTask.output.flush
                 tasksLog.info("Killed task: "+killedTask+" with value -1")
                 programResults.put(killedTask.id,-1)
                 tasksToKill -= taskToKillId
             })
           }
        }
      }

      //runs the nextp task in the ready queue, returns the task id of the running task
      def runNextTask:Unit = {
        //println("run next task")
        activeSchedulingQueue = queueToSchedule
        var executedInstruction = true

        //Console.println("queueToSchedule: "+queueToSchedule.toString)
        if (!activeSchedulingQueue.isEmpty || !waitingQueue.isEmpty){
          if (!activeSchedulingQueue.isEmpty){
            tasksLog.info("Executing tasks from queue: "+activeSchedulingQueue.getName)

            if (activeSchedulingQueue.size == 1 || activeSchedulingQueue.isPreemptive)
              currentTask = activeSchedulingQueue(0)

            //println("current task: "+currentTask)
            //set response time if it's not set
            if (currentTask.tickWhenStartedExecuting <0 )
              currentTask.tickWhenStartedExecuting=ticks.size

            //load registers from the task into the system
            currentTask.registers foreach{
              register => sys.registers.update(register._1,register._2)
            }

            currentTask.setStateTo("Running")

            //Console.println("ejecutando "+currentTask.name)
            while (!activeSchedulingQueue.isEmpty && !currentTask.executionHasFinished) {
              //check for tasks to kill
              checkForTasksToKill

              //check for quantum end
              if (activeSchedulingQueue.hasQuantum && (activeSchedulingQueue.getQuantum==timeOfTaskInCPU)){
                //quantum has end, context switch because of quantum
                doContextSwitch("quantum_end")
              }else{
                //execute normally
                val tickMetadata = new tickMeta(currentTask.userProgramInterpreter.getNextInstructionMeta(),activeSchedulingQueue,queues,waitingQueue,currentTask)
                ticks.put(ticks.size,tickMetadata)
                currentTask.timeInCPU +=1
                timeOfTaskInCPU +=1
                activeSchedulingQueue.timeInExecution +=1

                try {
                  currentTask.execNext()
                } catch{
                  case e:exceptions.executionException => {
                      println(e)
                      currentTask.executionHasFinished = true
                  }
                  case a => {
                      println(a.toString)
                  }
                }
                
                if (currentTask.userProgramInterpreter.didFork && queueToSchedule.isPreemptive){
                  //load registers from current task
                  tasksLog.info("running next task because of fork and "+queueToSchedule.getName+" is preemptive")
                  currentTask.saveRegisters(registers)
                  tasksLog.info("saved task registers")
                  runNextTask
                }

                updateAndCheckForWaitingQueue

                if (currentTask.contextSwitchFlag){
                  doContextSwitch("i/o")
                }

                
              }
            }
          } else{ //end if active scheduling queue is not empty
            //no execution was done, tick is not an instruction interpreted
            val tickMetadata = new tickMeta((0,"Waiting"),activeSchedulingQueue,queues,waitingQueue,null)
            ticks.put(ticks.size,tickMetadata)
            executedInstruction = false
          }
          //println("update waiting")
          updateAndCheckForWaitingQueue

          if (!queueToSchedule.isEmpty || !waitingQueue.isEmpty){
            //println("mando a run next task")
            runNextTask
          }else{
            //println("not running next task")
            //println("queue to schedule: "+queueToSchedule)
            //println("queue to schedule: "+waitingQueue)
          }

          } /*else{
          tasksLog.info("No tasks were found in any queue")
        }*/
      }

      def runTask(newTask:core.task) = {
        tasksLog.info("runTask: "+newTask.toString)
        newTask.tickWhenCreated = sys.scheduler.ticks.size
        tasksList.put(newTask.id,newTask)
        //add it to ready queue
        newTask.setStateTo("Ready")

        //place it in starting queue
        val startingSchedulingQueue = queues.find(_.getId==startQueue).get
        tasksLog("Adding "+newTask.toString+" to queue: "+startingSchedulingQueue.getName)
        startingSchedulingQueue.enQueue(newTask)
        queuesWhereTasksAre.put(newTask.id,startingSchedulingQueue)

        tasksLog.info("Added task to queue: "+startingSchedulingQueue.getName)

        if ((!startingSchedulingQueue.isEmpty || !queueToSchedule.isEmpty) && newTask.parentId==0){ //execute next task if and only if it was not a fork process i.e. its parent is 0
          runNextTask
        }

        newTask.id
      }

      def killTask(taskId:Int) = {
        sys.tasksLog.info("Attempting to kill task id: "+taskId.toString)
        val taskToKill = sys.getTask(taskId)
        val stateOfTask:Int = taskToKill.state

        if (stateOfTask<0)
          throw invalidTaskIdentifier(taskId," when attempting to kill it, it has already finished")

        tasksToKill += taskId
        sys.tasksLog.info("Added : "+taskToKill.toString+" to kill queue")
        sys.tasksLog("Kill queue: "+tasksToKill.toString)
      }

      def runProgram(parentTaskId:Int,programToExecute:programs.program,priority:Int,requiredFrames:Int,doVerbose:Boolean) = {
        tasksCounter += 1

        val newTask = new task(parentTaskId,getNewProcessId,programToExecute,registers,priority,requiredFrames,doVerbose)
        runTask(newTask)
      }

      def doContextSwitch(cause:String) = {
        tasksLog.info("Context switch, cause: "+cause)
        //println("Context switch, cause: "+cause)
        val taskToLeaveQueue = activeSchedulingQueue.deQueue
        //println("task to leave: "+taskToLeaveQueue)
        tasksLog.info("Dequeued task, active scheduling queue: "+activeSchedulingQueue.toString)
        taskToLeaveQueue.saveRegisters(registers)
        tasksLog.info("saved task registers")

        cause match{
          case "i/o" => {
              taskToLeaveQueue.setStateTo("waiting")
              queueToPlaceTaskAfterIOWait.put(taskToLeaveQueue.id,queues.find(_.getId == activeSchedulingQueue.getQueueOnIOEndId).get)
              tasksLog.info("Queue to place task after I/O wait: "+queueToPlaceTaskAfterIOWait.get(taskToLeaveQueue.id).get.toString)
              waitingQueue.enqueue(taskToLeaveQueue)
              tasksLog.info("Added task to waiting queue: "+taskToLeaveQueue.toString)
          }
          case "quantum_end" => {
              val queueOnQuantumEnd:schedulingQueue = queues.find(_.getId == activeSchedulingQueue.getQueueOnQuantumEndId).get
              tasksLog.info("Enqueued task: "+taskToLeaveQueue.toString+" in queue: "+queueOnQuantumEnd.toString)
              queueOnQuantumEnd.enQueue(taskToLeaveQueue)
              queuesWhereTasksAre.put(taskToLeaveQueue.id,queueOnQuantumEnd)
          }
        }

        tasksLog.info("queues: "+queues.toString)
        timeOfTaskInCPU = 0
        //currentTask = null
        //force to fetch next task if there is any
        //runNextTask
      }


      def endTask(endValue:Int){
        sys.tasksLog("Ending current task with value "+endValue.toString)
          val endingTask = activeSchedulingQueue.deQueue
          //free ram
          val framesOwnedByTask = pageOwners.zipWithIndex.filter(_._1==endingTask.id).map(_._2).toList
          memLog.info("ending task "+endingTask.id+", frames owned: "+framesOwnedByTask.mkString(","))

          framesOwnedByTask.foreach(frameIndex => pageOwners(frameIndex) = -1 )

          sys.tasksLog("Ending task: "+endingTask.toString)
          endingTask.executionHasFinished = true
          //val parentId = endingTask.parentId

          endingTask.setStateTo("Terminated")
          endingTask.output.flush

          tasksLog.info("Ended task: "+endingTask.toString+" with value "+endValue.toString)
          programResults.put(endingTask.id,endValue)
          tasksList.update(endingTask.id, endingTask)


          runNextTask
      }

     def act = {
        var newTaskId =
        loop{
          receive{
            case newTask:core.task =>{
               println("act ")
               runTask(newTask)
               println("termino act")
            }
          }
        }
            newTaskId
      }
    }

    var registers = scala.collection.mutable.Map[String,Int](
        "R0" -> 0, "R1" -> 0, "R2" -> 0, "R3" -> 0, "R4" -> 0, "R5" -> 0, "R6" -> 0, "R7" -> 0,
        "R8" -> 0, "R9" -> 0, "R10" -> 0, "R11" -> 0, "R12" -> 0, "R13" -> 0, "R14" -> 0, "R15" -> 0
    )

    def echo(message:String){
          shell.print(message)
    }

    def getBurstTime(anyUserProgram:programs.userProgram):Int = {
      anyUserProgram.parsedLines.size
    }

    def updateTaskInTasksList(taskId:Int,taskToUpdate:task) = 
      if (!shell.systemPrograms.contains(taskToUpdate.name))
        tasksList.update(taskId,taskToUpdate)

    def isRegister(name:String) = registers.contains(name.toUpperCase)

    def forkProcess(taskId:Int, command:List[String],output:outputMethod) = {
        val programMeta = shell.getProgramFromEntry(command.mkString(" "))
        val newTaskId = shell.exec(taskId,programMeta,tasksList.get(taskId).get.verbose)
        tasksLog.info("taskId "+taskId.toString+" has fork a new child, taskId: "+tasksList.get(newTaskId).get.toString)
        newTaskId
    }

    def isFileOpen(fileName:String):Boolean ={
        val fileIterator = fileNames.valuesIterator

        //filter the files knowing the lead term of the tuple is the path to the file
        val filteredFile = fileIterator.filter(_==fileName)

        if (filteredFile.hasNext)
        //file is indeed open
            true
        else
                false
    }

    def isFileInUseByAnotherTask(fileName:String,askingTaskId:Int):Boolean = {
        var fileIsBeingUsedByAnotherTask = false

        if (isFileOpen(fileName)){
            //file is indeed open, check the owner now
            val handlersIterator = fileNames.keySet.iterator

            while (handlersIterator.hasNext){
              val nextHandlerId = handlersIterator.next
              val fileOwner = fileOwners.get(nextHandlerId).get


            if (fileNames.get(nextHandlerId).get==fileName)
                if (fileOwner != askingTaskId)
                  fileIsBeingUsedByAnotherTask = true
            }
        }

        fileIsBeingUsedByAnotherTask
    }

    /**
    * System Calls
    */

    def openFile(taskId:Int,fileName:String):Int = {
        val pathToFile = shell.currentDir + fileName // <-- chapuz
        val handlerValue = getNewFileHandler

        val fileObject = new java.io.BufferedReader(new java.io.FileReader(pathToFile))
        ioLog.info("task "+tasksList.get(taskId).get+" has opened \""+fileName+"\", handler assigned :"+handlerValue.toString)
        files.put(handlerValue,fileObject)
        fileOwners.put(handlerValue,taskId)
        fileNames.put(handlerValue,fileName)

        //yield handler Value
        handlerValue
    }

    def closeFile(taskId:Int,handlerId:Int) {
        if (files.contains(handlerId)){
                val fileToClose = files.get(handlerId).get
                val fileTaskOwner = fileOwners.get(handlerId).get
                val fileName = fileNames.get(handlerId).get

                if (fileTaskOwner == taskId){
                  fileToClose.close
                  //remove the file
                  files - handlerId
                  fileOwners - handlerId
                  fileNames - handlerId
                  ioLog.info("task "+fileTaskOwner+" has closed the file with handler id "+handlerId.toString+" named:"+fileName)
                }else
                  throw new permissionToFileDeniedException(tasksList.get(taskId).get,tasksList.get(fileTaskOwner).get,handlerId)
        } else
                throw new invalidHandlerIdException(handlerId)
    }

    def readLineFromFile(taskId:Int,handlerId:Int) = {
        if (files.contains(handlerId)){
          val fileTaskOwner = fileOwners.get(handlerId).get

          if (fileTaskOwner == taskId){
                  val fileObject = files.get(handlerId).get
                  var readLine = fileObject.readLine

                  ioLog.info("taskId "+taskId.toString+" has read a line from the file with handler id "+handlerId.toString)
                  readLine //return the read line
          }
          else
                  throw new permissionToFileDeniedException(tasksList.get(taskId).get,tasksList.get(fileTaskOwner).get,handlerId)
        } else
                throw new invalidHandlerIdException(handlerId)
    }

    def getStateValue(taskId:Int):Int = {
      tasksList.get(taskId).get.state
    }

    def getEndValue(taskId:Int):Int = {
      if (programResults.contains(taskId))
              programResults.get(taskId).get
      else{
              if (tasksList.contains(taskId))
                      0
              else
                      -1
      }
    }
    /**
    * Funcionality
    */
    def getOutput(isAsyncronous:Boolean, pathToFile:String) = new outputMethod(isAsyncronous,pathToFile)

    def getNewFileHandler = files.size+1
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

    object stats{
      import java.net._
      import java.io.{BufferedReader,BufferedWriter,InputStreamReader}

      var htmlPage:core.outputMethod = null

      def generate(statsFolder:String,output:core.outputMethod) = {
         htmlPage = output
        /**
         * Setup html page
         */
         htmlPage.println("<html>")

         /**
          * Overview
          */
         htmlPage.println("<h2>Overview</h2>")

         htmlPage.print("<h3>Throughput</h3>")
         val terminatedTasks:Float = tasksList.filter(_._2.state==5).size
         htmlPage.print("Terminated tasks: "+terminatedTasks.toString+"</br>")
         var totalTimeInCPU:Float = 0
         sys.scheduler.queues.foreach(queue =>{
             totalTimeInCPU += queue.timeInExecution
         })
         htmlPage.print("Number of ticks: "+totalTimeInCPU.toString+"<br>")
         htmlPage.print("<b>Throughput: </b>"+(terminatedTasks/totalTimeInCPU).toString)

         htmlPage.print("<h3>CPU Utilization</h3>")
         htmlPage.print("Number of ticks in CPU: "+totalTimeInCPU.toString+"</br>")
         var totalWaitingTime:Float = sys.scheduler.totalTimeOfWaitingQueue
         htmlPage.print("Number of ticks just waiting for I/O or semaphores: "+totalWaitingTime.toString+"</br>")
         htmlPage.print("<b>CPU Utilization: </b> "+(totalTimeInCPU/(totalTimeInCPU+totalWaitingTime)).toString+"%</br>")


         var imgName = "overview_executionTimeByQueues.png"
         var title = "Execution time by Queues"
         htmlPage.print("<h3>Execution time by Queues </h3>")
         var url = getPieChartRequest(getData_OverviewExecutionTimeByQueue(imgName),title)
         putPNG(statsFolder+imgName,url)
         htmlPage.print("<center><img src='"+statsFolder+imgName+"'/></center>")

         htmlPage.print("</br></br>")
         imgName = "overview_executionTimeByTasks.png"
         title = "Execution time by Tasks"

         htmlPage.print("<h3>Execution time by Tasks</h3>")
         url = getPieChartRequest(getData_OverviewExecutionTimeByTasks(imgName),title)
         putPNG(statsFolder+imgName,url)
         htmlPage.print("<center><img src='"+statsFolder+imgName+"'/></center>")
         htmlPage.print("</br></br>")

         imgName = "overview_waitingTimeByTasks.png"
         title = "Waiting time by Tasks"
         htmlPage.print("<h3>Waiting time by Tasks </h3>")
         url = getPieChartRequest(getData_OverviewWaitingTimeByTasks(imgName),title)
         putPNG(statsFolder+imgName,url)
         htmlPage.print("<center><img src='"+statsFolder+imgName+"'/></center>")
         htmlPage.print("</br></br>")

         imgName = "overview_TurnaroundTimeByTasks.png"
         title = "Turnaround time by Tasks"
         htmlPage.print("<h3>Turnaround time by Tasks </h3>")
         url = getPieChartRequest(getData_OverviewTurnaroundTimeByTasks(imgName),title)
         putPNG(statsFolder+imgName,url)
         htmlPage.print("<center><img src='"+statsFolder+imgName+"'/></center>")
         htmlPage.print("</br></br>")

        imgName = "overview_ResponseTimeByTasks.png"
         title = "Response time by Tasks"
         htmlPage.print("<h3>Response time by Tasks </h3>")
         url = getPieChartRequest(getData_OverviewResponseTimeByTasks(imgName),title)
         putPNG(statsFolder+imgName,url)
         htmlPage.print("<center><img src='"+statsFolder+imgName+"'/></center>")
         htmlPage.print("</br></br>")

         /**
          * For each tick
          */
          htmlPage.println("<h2>Ticks</h2>")

          sys.scheduler.ticks.toList.sortBy(_._1).foreach(tick =>{
            val tickNumber:Int = tick._1
            val metaData:tickMeta = tick._2
            htmlPage.println("<h3>Tick: "+tickNumber.toString+"</h3>")
            htmlPage.println("<b>Instruction:</b> "+metaData.getInstruction._2+" <b>in line</b> "+metaData.getInstruction._1.toString+"</br>")
            htmlPage.println("<b>Scheduling queue:</b> "+metaData.activeQueueString+"</br>")
            htmlPage.println("<b>Waiting queue:</b> "+metaData.waitingQueueString+"</br>")
            htmlPage.println("<b>Queues:</b> "+metaData.queuesString+"</br>")
            if (metaData.getTask!=null)
              htmlPage.println("<b>Task:</b> "+metaData.getTask.toString+"</br>")
          })
         /**
          * End html page
          */
      }

      def getData_OverviewExecutionTimeByQueue(imgName:String):scala.collection.mutable.Map[String,Float] = {
        var totalTimeInCPU:Float = 0
         sys.scheduler.queues.foreach(queue =>{
             totalTimeInCPU += queue.timeInExecution
         })

         htmlPage.print("<b>Total time in CPU for queues:</b> "+totalTimeInCPU.toString+"</br>")
         var data:scala.collection.mutable.Map[String,Float] = scala.collection.mutable.Map[String,Float]()

         sys.scheduler.queues.toList.sortBy(_.getId).foreach(queue =>{
             val timeInExecutionOfQueue = queue.timeInExecution
             htmlPage.print("Time of execution of "+queue.getName+": "+timeInExecutionOfQueue.toString)
             val percentage:Float = timeInExecutionOfQueue/totalTimeInCPU
             htmlPage.print(" i.e. "+percentage.toString+"% of the total </br>")
             data.put(queue.getName, percentage)
         })

         data
      }

      def getData_OverviewExecutionTimeByTasks(imgName:String):scala.collection.mutable.Map[String,Float] = {
        var totalTimeOfCPUUsed:Float = 0
         sys.scheduler.queues.foreach(queue =>{
             totalTimeOfCPUUsed += queue.timeInExecution
         })

         htmlPage.print("<b>Total time in CPU for tasks:</b> "+totalTimeOfCPUUsed.toString+"</br>")
         var data:scala.collection.mutable.Map[String,Float] = scala.collection.mutable.Map[String,Float]()

        sys.tasksList.toList.sortBy(_._1).foreach(task =>{
            val taskId = task._1
            val pureTask = task._2

            htmlPage.print("Time of execution of "+taskId.toString +": "+pureTask.name+": "+pureTask.timeInCPU.toString)
            val percentage:Float = pureTask.timeInCPU/totalTimeOfCPUUsed
            htmlPage.print(" i.e. "+percentage.toString+"% of the total </br>")
            data.put(taskId.toString +": "+pureTask.name, percentage)
          })

         data
      }

      def getData_OverviewWaitingTimeByTasks(imgName:String):scala.collection.mutable.Map[String,Float] = {
        var totalWaitingTime:Float = sys.scheduler.totalTimeOfWaitingQueue

         htmlPage.print("<b>Total time in waiting time for tasks:</b> "+totalWaitingTime.toString+"</br>")
         var data:scala.collection.mutable.Map[String,Float] = scala.collection.mutable.Map[String,Float]()

         sys.tasksList.toList.sortBy(_._1).foreach(task =>{
            val taskId = task._1
            val pureTask = task._2

            htmlPage.print("Waiting time of "+taskId.toString +": "+pureTask.name+": "+pureTask.waitingTime.toString)
            val percentage:Float = pureTask.waitingTime/totalWaitingTime
            htmlPage.print(" i.e. "+percentage.toString+"% of the total </br>")
            data.put(taskId.toString +": "+pureTask.name, percentage)
          })
          htmlPage.print("Average: "+(totalWaitingTime/sys.tasksList.size).toString+"</br>")

         data
      }

      def getData_OverviewResponseTimeByTasks(imgName:String):scala.collection.mutable.Map[String,Float] = {
        var totalResponseTime:Float = 0
        tasksList.foreach(task =>{
            totalResponseTime += task._2.responseTime
        })

         htmlPage.print("<b>Total response time for tasks:</b> "+totalResponseTime.toString+"</br>")
         var data:scala.collection.mutable.Map[String,Float] = scala.collection.mutable.Map[String,Float]()

         sys.tasksList.toList.sortBy(_._1).foreach(task =>{
            val taskId = task._1
            val pureTask = task._2

            val responseTime = pureTask.responseTime
            htmlPage.print("Response time time of "+taskId.toString +": "+pureTask.name+": "+responseTime.toString)
            val percentage:Float = responseTime/totalResponseTime
            htmlPage.print(" i.e. "+percentage.toString+"% of the total </br>")
            data.put(taskId.toString +": "+pureTask.name, percentage)
          })

         htmlPage.print("Average: "+(totalResponseTime/sys.tasksList.size).toString+"</br>")
         data
      }

      def getData_OverviewTurnaroundTimeByTasks(imgName:String):scala.collection.mutable.Map[String,Float] = {
        var totalTurnaroundTime:Float = 0
         sys.scheduler.queues.foreach(queue =>{
             totalTurnaroundTime += queue.timeInExecution
         })

        totalTurnaroundTime += sys.scheduler.totalTimeOfWaitingQueue

         htmlPage.print("<b>Total turnaround for tasks:</b> "+totalTurnaroundTime.toString+"</br>")
         var data:scala.collection.mutable.Map[String,Float] = scala.collection.mutable.Map[String,Float]()

        sys.tasksList.toList.sortBy(_._1).foreach(task =>{
            val taskId = task._1
            val pureTask = task._2

            val turnAroundTime = pureTask.timeInCPU + pureTask.waitingTime
            htmlPage.print("Turnaround time of "+taskId.toString +": "+pureTask.name+": "+turnAroundTime.toString)
            val percentage:Float = turnAroundTime/totalTurnaroundTime
            htmlPage.print(" i.e. "+percentage.toString+"% of the total </br>")
            data.put(taskId.toString +": "+pureTask.name, percentage)
          })

         htmlPage.print("Average: "+(totalTurnaroundTime/sys.tasksList.size).toString+"</br>")
         data
      }

      def getPieChartRequest(data:scala.collection.mutable.Map[String,Float],title:String):String={
        val request = "http://chart.apis.google.com/chart?cht=p3"

        var chd = "&chd=t:" //slices percentages
        var chl = "&chl=" //slices labels
        var chs = "&chs=700x300"
        var chtt = "&chtt="+title

        data.foreach(slice =>{
            val label = slice._1
            val percentage = slice._2
            if (data.last==slice){
              chd += percentage.toString
              chl += label
            }
            else{
              chd += percentage.toString + ","
              chl += label + "|"
            }
        }
        )

        val requestWithNoSpaces = (request+chd+chs+chl+chtt).replaceAll(" ", "%20")
        requestWithNoSpaces
      }

      def putPNG(pathToFile:String,url:String) ={
        val input = scala.io.Source.fromURL(new URL(url))
        val output = new java.io.PrintStream(new java.io.FileOutputStream(pathToFile))
        while(input.hasNext)
           output.print(input.next)

        output.flush
        output.close
        input.close
      }

      def getOverallQueueExecutionTime = {

      }
    }
  }
}




