/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package util
import core.task

class schedulingQueue(id:Int,name:String,typeOfQueue:String,quantum:Int,preemptive:Boolean,queueOnQuantumEnd:Int,queueOnIOEnd:Int){
  var pQueue = new scala.collection.mutable.PriorityQueue[core.task]()(Ordering.fromLessThan(_.priority < _.priority))
  var sjfQueue = new scala.collection.mutable.PriorityQueue[core.task]()(Ordering.fromLessThan(_.burstTime < _.burstTime))
  var fcfsQueue = new scala.collection.mutable.Queue[core.task]()

  var timeInExecution:Float = 0
  
  var Queue:Seq[core.task] = typeOfQueue.toUpperCase match{
    case "FCFS" => fcfsQueue
    case "PRIORITY" => pQueue
    case "SJF" => sjfQueue
  }

  def deQueue:task = {
    typeOfQueue.toUpperCase match{
      case "FCFS" => fcfsQueue.dequeue
      case "PRIORITY" => pQueue.dequeue
      case "SJF" => sjfQueue.dequeue
    }
  }

  def enQueue(elem:task) = {
    typeOfQueue.toUpperCase match{
      case "FCFS" => fcfsQueue.enqueue(elem)
      case "PRIORITY" => pQueue.enqueue(elem)
      case "SJF" => sjfQueue.enqueue(elem)
    }
  }

  def isEmpty = typeOfQueue.toUpperCase match{
      case "FCFS" => fcfsQueue.isEmpty
      case "PRIORITY" => pQueue.isEmpty
      case "SJF" => sjfQueue.isEmpty
  }

  def size:Int = typeOfQueue.toUpperCase match{
      case "FCFS" => fcfsQueue.size
      case "PRIORITY" => pQueue.size
      case "SJF" => sjfQueue.size
  }
  
  def hasQuantum = quantum > 0

  def apply(index:Int):core.task = Queue(index)

  def getId:Int = id
  
  def getName:String = name

  def getType:String = typeOfQueue

  def getQuantum:Int = quantum

  def getQueueOnQuantumEndId:Int = queueOnQuantumEnd

  def getQueueOnIOEndId:Int = queueOnIOEnd

  def isPreemptive = preemptive
  
  def getSize = Queue.size
  
  override def toString = "id: "+id.toString+" name: "+name +" quantum: "+quantum.toString+" Queue: " +Queue.toString
}
