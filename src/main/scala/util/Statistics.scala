package util

import java.net._
import java.io.{BufferedReader,BufferedWriter,InputStreamReader,File}
import core.OperatingSystem

class Statistics(os:OperatingSystem){

  var htmlPage:core.outputMethod = null

  def generate(statsFolder:String,output:core.outputMethod) = {

    if (!new File(statsFolder).exists)
      new File(statsFolder).mkdirs

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
    val terminatedTasks:Float = os.tasks.filter(_._2.state==5).size
    htmlPage.print("Terminated tasks: "+terminatedTasks.toString+"</br>")
    var totalTimeInCPU:Float = 0
    os.scheduler.queues.foreach(queue =>{
      totalTimeInCPU += queue.timeInExecution
    })
    htmlPage.print("Number of ticks: "+totalTimeInCPU.toString+"<br>")
    htmlPage.print("<b>Throughput: </b>"+(terminatedTasks/totalTimeInCPU).toString)

    htmlPage.print("<h3>CPU Utilization</h3>")
    htmlPage.print("Number of ticks in CPU: "+totalTimeInCPU.toString+"</br>")
    var totalWaitingTime:Float = os.scheduler.totalTimeOfWaitingQueue
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

    os.scheduler.ticks.toList.sortBy(_._1).foreach(tick =>{
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
  } //end generate

  def getData_OverviewExecutionTimeByQueue(imgName:String):scala.collection.mutable.Map[String,Float] = {
      var totalTimeInCPU:Float = 0
      os.scheduler.queues.foreach(queue =>{
        totalTimeInCPU += queue.timeInExecution
      })

      htmlPage.print("<b>Total time in CPU for queues:</b> "+totalTimeInCPU.toString+"</br>")
      var data:scala.collection.mutable.Map[String,Float] = scala.collection.mutable.Map[String,Float]()

      os.scheduler.queues.toList.sortBy(_.getId).foreach(queue =>{
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
      os.scheduler.queues.foreach(queue =>{
        totalTimeOfCPUUsed += queue.timeInExecution
      })

      htmlPage.print("<b>Total time in CPU for tasks:</b> "+totalTimeOfCPUUsed.toString+"</br>")
      var data:scala.collection.mutable.Map[String,Float] = scala.collection.mutable.Map[String,Float]()

      os.tasks.toList.sortBy(_._1).foreach(task =>{
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
      var totalWaitingTime:Float = os.scheduler.totalTimeOfWaitingQueue

      htmlPage.print("<b>Total time in waiting time for tasks:</b> "+totalWaitingTime.toString+"</br>")
      var data:scala.collection.mutable.Map[String,Float] = scala.collection.mutable.Map[String,Float]()

      os.tasks.toList.sortBy(_._1).foreach(task =>{
        val taskId = task._1
        val pureTask = task._2

        htmlPage.print("Waiting time of "+taskId.toString +": "+pureTask.name+": "+pureTask.waitingTime.toString)
        val percentage:Float = pureTask.waitingTime/totalWaitingTime
        htmlPage.print(" i.e. "+percentage.toString+"% of the total </br>")
        data.put(taskId.toString +": "+pureTask.name, percentage)
      })
      htmlPage.print("Average: "+(totalWaitingTime/os.tasks.size).toString+"</br>")

      data
    }

    def getData_OverviewResponseTimeByTasks(imgName:String):scala.collection.mutable.Map[String,Float] = {
      var totalResponseTime:Float = 0
      os.tasks.foreach(task =>{
        totalResponseTime += task._2.responseTime
      })

      htmlPage.print("<b>Total response time for tasks:</b> "+totalResponseTime.toString+"</br>")
      var data:scala.collection.mutable.Map[String,Float] = scala.collection.mutable.Map[String,Float]()

      os.tasks.toList.sortBy(_._1).foreach(task =>{
        val taskId = task._1
        val pureTask = task._2

        val responseTime = pureTask.responseTime
        htmlPage.print("Response time time of "+taskId.toString +": "+pureTask.name+": "+responseTime.toString)
        val percentage:Float = responseTime/totalResponseTime
        htmlPage.print(" i.e. "+percentage.toString+"% of the total </br>")
        data.put(taskId.toString +": "+pureTask.name, percentage)
      })

      htmlPage.print("Average: "+(totalResponseTime/os.tasks.size).toString+"</br>")
      data
    }

    def getData_OverviewTurnaroundTimeByTasks(imgName:String):scala.collection.mutable.Map[String,Float] = {
      var totalTurnaroundTime:Float = 0
      os.scheduler.queues.foreach(queue =>{
        totalTurnaroundTime += queue.timeInExecution
      })

      totalTurnaroundTime += os.scheduler.totalTimeOfWaitingQueue

      htmlPage.print("<b>Total turnaround for tasks:</b> "+totalTurnaroundTime.toString+"</br>")
      var data:scala.collection.mutable.Map[String,Float] = scala.collection.mutable.Map[String,Float]()

      os.tasks.toList.sortBy(_._1).foreach(task =>{
        val taskId = task._1
        val pureTask = task._2

        val turnAroundTime = pureTask.timeInCPU + pureTask.waitingTime
        htmlPage.print("Turnaround time of "+taskId.toString +": "+pureTask.name+": "+turnAroundTime.toString)
        val percentage:Float = turnAroundTime/totalTurnaroundTime
        htmlPage.print(" i.e. "+percentage.toString+"% of the total </br>")
        data.put(taskId.toString +": "+pureTask.name, percentage)
      })

      htmlPage.print("Average: "+(totalTurnaroundTime/os.tasks.size).toString+"</br>")
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
}
