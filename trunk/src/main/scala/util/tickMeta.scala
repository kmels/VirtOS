/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package util
import core.Task
import scala.collection.mutable.{ArrayBuffer,Queue}

class tickMeta(instruction:(Int,String),activeQueue:util.schedulingQueue,queues:Seq[schedulingQueue],waitingQueue:Queue[Task],whichTask:Task) {

  def getInstruction = instruction

  var activeQueueString = activeQueue.toString
  var queuesString = queues.toString
  var waitingQueueString = waitingQueue.toString
  
  def getTask = whichTask

  override def toString = instruction._2
}
