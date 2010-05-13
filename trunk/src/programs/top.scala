/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package programs

import core.sys

class top(outputObject:core.outputMethod) extends system_program{
  val programName = "ls"
  val number_of_max_params = 0
  val output = outputObject

  def exec() ={
    output.println("Memory: ")
    val memoryUsed = sys.pageOwners.filter(_> (-1)).size
    val freeMemory = sys.pageOwners.filter(_ < 0).size
    output.println("used: "+memoryUsed+" of "+sys.ramSize+"("+(memoryUsed*100/sys.ramSize).toString+"%)")
    output.println("free: "+freeMemory+" of "+sys.ramSize+"("+(freeMemory*100/sys.ramSize).toString+"%)")
    output.println("frame #\t\t\towner\t\t\tvalues")
    sys.memory.zipWithIndex.foreach(frame => {
        val frameNumber:Int = frame._2
        val frameValues:scala.collection.mutable.Buffer[Int] = frame._1
        val frameOwner = sys.pageOwners(frameNumber)
        output.println(frameNumber.toString+"\t\t\t"+frameOwner.toString+"\t\t\t"+frameValues.mkString(","))
    })

    output.println("Swap: ")
    val swapUsed = sys.swapOwners.filter(_> (-1)).size
    val freeSwap = sys.swapOwners.filter(_ < 0).size
    output.println("used: "+swapUsed+" of "+sys.ramSize+"("+(swapUsed*100/sys.ramSize)+"%)")
    output.println("free: "+freeSwap+" of "+sys.ramSize+"("+(freeSwap*100/sys.ramSize)+"%)")
    output.println("frame #\t\t\towner\t\t\tvalues")
    sys.swap.zipWithIndex.foreach(swapFrame => {
        val frameNumber:Int = swapFrame._2
        val frameValues:scala.collection.mutable.Buffer[Int] = swapFrame._1
        val frameOwner = sys.swapOwners(frameNumber)
        output.println(frameNumber.toString+"\t\t\t"+frameOwner.toString+"\t\t\t"+frameValues.mkString(","))
    })
  }
}