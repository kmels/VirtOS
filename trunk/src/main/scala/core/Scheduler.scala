package core

import scala.actors._
import scala.actors.Actor._
import util.{schedulingQueue,tickMeta,Log}
import scala.collection.immutable.{ListSet}
import scala.collection.mutable.{ArrayBuffer,Queue}
import exceptions.{invalidTaskIdentifier,frameTooLargeException}
import programs.userProgram

class Scheduler(os:OperatingSystem) extends Actor{
  val tasksLog = new Log(os.pathToLogs+"ps.log")
  tasksLog.info("Tasks log initialized")
  
  //load queue configuration
  tasksLog("Loading configuration")
  val queues:List[schedulingQueue] = os.configuration.queues.toList
  tasksLog("Loaded Queue configuration: "+queues.mkString(","))
  var startQueue:Int = os.configuration.startQueue
  tasksLog("Loaded startQueue="+startQueue.toString)

  val IOqueues = scala.collection.mutable.Map[String,Queue[Int]]()
//  val programResults = scala.collection.mutable.Map[Int,Int]()
  var activeSchedulingQueue:schedulingQueue = null
  var queueToPlaceTaskAfterIOWait = scala.collection.mutable.Map[Int,schedulingQueue]() //id,queueonQuantumEnd
  var tasksToKill:ListSet[Int] = new ListSet()
  
  var currentTask:Task = null
  
  var ticks = scala.collection.mutable.Map[Int,tickMeta]()
  var timeOfTaskInCPU:Int = 0
  var waitingQueue:Queue[Task] = new Queue()
  var totalTimeOfWaitingQueue=0
  var queuesWhereTasksAre = scala.collection.mutable.Map[Int,schedulingQueue]() //task id, scheduling queue
  
  
  /**
   * Schedules a new task
   */
  //def schedule(task:Task):Unit = {
    
  //}

  def getNewTask(parentTaskId:Int,programToExecute:programs.program,priority:Int,pagesNeeded:Int,doVerbose:Boolean) = {
    new Task(os,parentTaskId,getNewProcessId,programToExecute,os.registers,priority,pagesNeeded,doVerbose)
  }
  
  def queueToSchedule:schedulingQueue ={
    val startingSchedulingQueue = queues.find(_.getId==startQueue).get

    if (!startingSchedulingQueue.isEmpty){
      //if the start queue is not empty, we've to execute its tasks'
      startingSchedulingQueue
    }
    else{
      val nonEmptyQueues = queues.dropWhile(_.getSize==0)
      if (nonEmptyQueues.size>0){
        // the first one that matches
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
      tasksLog("Found something in tasks to kill list: "+tasksToKill.toString)
      //check if the current process is meant to be killed
      if (tasksToKill.contains(currentTask.id)){
        currentTask.executionHasFinished = true
        //remove from the tasks to kill
        tasksToKill -= currentTask.id
        tasksLog("Killing current task with value -1")

        //free ram
        val framesOwnedByTaskToKill = os.pageOwners.zipWithIndex.filter(_._1==currentTask.id).map(_._2).toList
        os.memLog.info("Killing Task "+currentTask.id+", frames owned: "+framesOwnedByTaskToKill.mkString(","))

        framesOwnedByTaskToKill.foreach(frameIndex => os.pageOwners(frameIndex) = -1 )

        //remove from the queue (kill it)
        val deQueuedTask = activeSchedulingQueue.deQueue

        currentTask.setStateTo("Terminated")
        //flush the tasks' output
        currentTask.output.flush

        tasksLog.info("Killed task: "+currentTask+" with value -1")
        os.programResults.put(currentTask.id,-1)
        //return -1
      }else{
        //something to kill in the ready queue
        tasksToKill foreach(taskToKillId => {
        tasksLog("Killing task id "+taskToKillId+" with value -1")

          //free ram
          val framesOwnedByTaskToKill = os.pageOwners.zipWithIndex.filter(_._1==taskToKillId).map(_._2).toList
          os.memLog.info("killing task "+taskToKillId+", frames owned: "+framesOwnedByTaskToKill.mkString(","))

          framesOwnedByTaskToKill.foreach(frameIndex => os.pageOwners(frameIndex) = -1 )

          val queueWhereTaskIs = queuesWhereTasksAre.get(taskToKillId).get

          val killedTask:Task =
            if (queueWhereTaskIs.Queue.exists(_.id == taskToKillId))
              //then it's ready
              queueWhereTaskIs.deQueue
            else
              waitingQueue.dequeueFirst(_.id == taskToKillId).get

          killedTask.setStateTo("Terminated")
          killedTask.output.flush
          tasksLog.info("Killed task: "+killedTask+" with value -1")
          os.programResults.put(killedTask.id,-1)
          tasksToKill -= taskToKillId
        })
      }
    }
  } //end check for tasks to kill

  def runNextTask:Unit = {
    activeSchedulingQueue = queueToSchedule
    var executedInstruction = true

    if (!activeSchedulingQueue.isEmpty || !waitingQueue.isEmpty){
      if (!activeSchedulingQueue.isEmpty){
        tasksLog.info("Executing tasks from queue: "+activeSchedulingQueue.getName)

        if (activeSchedulingQueue.size == 1 || activeSchedulingQueue.isPreemptive)
          currentTask = activeSchedulingQueue(0)

        //set response time if it's not set
        if (currentTask.tickWhenStartedExecuting <0 )
          currentTask.tickWhenStartedExecuting=ticks.size

        //load registers from the task into the system
        currentTask.registers foreach{
          register => os.registers.update(register._1,register._2)
        }

        currentTask.setStateTo("Running")
        
        while (!activeSchedulingQueue.isEmpty && !currentTask.executionHasFinished) {
//          println("while (!activeSchedulingQueue.isEmpty && !currentTask.executionHasFinished")
          //check for tasks to kill
          checkForTasksToKill

          //check for quantum end
          if (activeSchedulingQueue.hasQuantum && (activeSchedulingQueue.getQuantum==timeOfTaskInCPU)){
//            println("timeOfTaskINCPU: "+timeOfTaskInCPU)
            //quantum has end, context switch because of quantum
            doContextSwitch("quantum_end")
          }else{
  //          println("executing normally")
            //execute normally
            val tickMetadata = new tickMeta(currentTask.userProgramInterpreter.getNextInstructionMeta(),activeSchedulingQueue,queues,waitingQueue,currentTask)
            ticks.put(ticks.size,tickMetadata)
//            println("tickMeta: "+tickMetadata.toString)
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
              currentTask.saveRegisters(os.registers)
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
        //not running anything
      }

    } /*else{
    tasksLog.info("No tasks were found in any queue")
    }*/
  } //end run next task

  def runTask(newTask:Task) = {
    //println("task a correr: "+newTask)
    tasksLog.info("runTask: "+newTask.toString)
    newTask.tickWhenCreated = ticks.size
    os.tasks.put(newTask.id,newTask)
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

  def runProgram(parentTaskId:Int,programToExecute:userProgram,priority:Int,requiredFrames:Int,doVerbose:Boolean):Int = {
    try {
      val newTask = new Task(os,parentTaskId,getNewProcessId,programToExecute,os.registers,priority,requiredFrames,doVerbose)
      tasksLog.info("Created task id: "+newTask.id+", priority: "+priority+", frames: "+requiredFrames)
      runTask(newTask)
    } catch{
      case invalidFrameSize:frameTooLargeException => os.shell.println(invalidFrameSize.toString); -1
    }
  }

  def doContextSwitch(cause:String) = {
    tasksLog.info("Context switch, cause: "+cause)
    //println("Context switch, cause: "+cause)
    val taskToLeaveQueue = activeSchedulingQueue.deQueue
    //println("task to leave: "+taskToLeaveQueue)
    tasksLog.info("Dequeued task, active scheduling queue: "+activeSchedulingQueue.toString)
    taskToLeaveQueue.saveRegisters(os.registers)
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
//        println("tick: "+ticks.size+" haciendo cntxtswtch")
        val queueOnQuantumEnd:schedulingQueue = queues.find(_.getId == activeSchedulingQueue.getQueueOnQuantumEndId).get
        tasksLog.info("Enqueued task: "+taskToLeaveQueue.toString+" in "+queueOnQuantumEnd.getName)
        queueOnQuantumEnd.enQueue(taskToLeaveQueue)
        queuesWhereTasksAre.put(taskToLeaveQueue.id,queueOnQuantumEnd)
      }
    }

    tasksLog.info("queues: "+queues.toString)
    timeOfTaskInCPU = 0
    //currentTask = null
    //force to fetch next task if there is any
    //runNextTask
  } //end doContextSwitch

  def endTask(endValue:Int){
    tasksLog("Ending current task with value "+endValue.toString)
    val endingTask = activeSchedulingQueue.deQueue
    //free ram
    val framesOwnedByTask = os.pageOwners.zipWithIndex.filter(_._1==endingTask.id).map(_._2).toList
    os.memLog.info("ending task "+endingTask.id+", frames owned: "+framesOwnedByTask.mkString(","))

    framesOwnedByTask.foreach(frameIndex => os.pageOwners(frameIndex) = -1 )

    tasksLog("Ending task: "+endingTask.toString)
    endingTask.executionHasFinished = true
    //val parentId = endingTask.parentId

    endingTask.setStateTo("Terminated")
    endingTask.output.flush

    tasksLog.info("Ended task: "+endingTask.toString+" with value "+endValue.toString)
    os.programResults.put(endingTask.id,endValue)
    os.tasks.update(endingTask.id, endingTask)
    runNextTask
  }

  def act = {
    var newTaskId =
      loop{
        receive{
          case newTask:Task=>{
            runTask(newTask)
          }
        }
      }
    newTaskId
  }

  def killTask(taskId:Int) = {
    tasksLog.info("Attempting to kill task id: "+taskId.toString)
    val taskToKill = os.getTask(taskId)
    val stateOfTask:Int = taskToKill.state

    if (stateOfTask<0)
      throw invalidTaskIdentifier(taskId," when attempting to kill it, it has already finished")

    tasksToKill += taskId
    tasksLog.info("Added : "+taskToKill.toString+" to kill queue")
    tasksLog("Kill queue: "+tasksToKill.toString)
  } //end kill task

  def getNewProcessId:Int = os.tasks.size+1

  def getFramesForNewTask(howMany:Int,taskId:Int,parentId:Int):List[Int] = {
    os.memLog.info("task "+taskId.toString+" is asking for "+howMany.toString+" frames")

    if (os.pageSize<howMany)
      throw new frameTooLargeException(taskId,howMany,os.pageSize)

    val assignedFrames:List[Int] =
      if (parentId!=0){
        os.memLog.info("task's parent found: "+parentId.toString+", sharing frames")
        os.tasks(parentId).frames.map(_._2).toList
      } else{
        val emptyPages = os.pageOwners.zipWithIndex.filter(_._1<0)
        os.memLog.info(emptyPages.size+" free frames were found: "+emptyPages.map(_._2).toList.mkString(","))
        if (emptyPages.size>=howMany){
          os.memLog.info("assigning free frames")
          val emptyFramesAssigned:List[Int] = emptyPages.slice(0, howMany).map(assignedPage => assignedPage._2).toList
          emptyFramesAssigned
        } else{
          os.memLog.info("trying to swap in/out")
          val swapUsed = os.swapOwners.find(_ > (-1)).size
          os.memLog.info("swap used: "+swapUsed.toString)
          val freeSwap = os.swapSize - swapUsed
          val freeSwapFramesIndexes = os.swapOwners.zipWithIndex.filter(_._1 < 0).map(_._2)
          os.memLog.info("free swap frames: "+freeSwapFramesIndexes.mkString(","))

          val pagesNotOwnedByCurrentTask:List[Int] = os.pageOwners.zipWithIndex.filter(_._1!=taskId).map(frame => frame._2).toList
          val swapedOutFrames = pagesNotOwnedByCurrentTask.slice(0,howMany)
          os.memLog.info("frames not owned by task asking for frames: "+pagesNotOwnedByCurrentTask.mkString(","))

          if (swapUsed < os.swapSize && freeSwap >= swapedOutFrames.size) {
            //swap out some
            os.memLog.info("swaping in and out frames")            
            os.memLog.info("swaping out frames from memory: "+swapedOutFrames.mkString(","))
            //falta poner el taskid.frames mapeadas a swap
            swapedOutFrames.foreach(swapedOutFrame => {
              //find first which is free
              val freeSwapFrames = os.swapOwners.zipWithIndex.filter(_._1 < 0)
              val freeSwapFrame = freeSwapFrames.head
              val indexToUpdate = freeSwapFrame._2
              os.swapOwners(indexToUpdate) = os.pageOwners(swapedOutFrame)
              os.memLog.info("swaping in swap frame "+indexToUpdate)
              os.swap(indexToUpdate) = os.memory(swapedOutFrame)
            })

            swapedOutFrames
          } else{
            os.memLog.info("not enough swap space")
            //return empty list
            List()
          }
        }
      }

    os.memLog.info("assigned frames: "+assignedFrames.mkString(","))
    assignedFrames.foreach(assignedPage => os.pageOwners(assignedPage) = taskId)
    os.memLog.info("frame owners table updated")
    os.memLog.info("memory owners: "+os.pageOwners.mkString(","))
    os.memLog.info("swap owners: "+os.swapOwners.mkString(","))
    assignedFrames
  }
}
