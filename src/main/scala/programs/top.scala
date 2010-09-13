/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package programs

import core.OperatingSystem

class top(os:OperatingSystem,outputObject:core.outputMethod) extends system_program{
  val programName = "ls"
  val number_of_max_params = 0
  val output = outputObject

  def exec() ={
    output.println("Memory: ")
    val memoryUsed = os.pageOwners.filter(_> (-1)).size
    val freeMemory = os.pageOwners.filter(_ < 0).size
    output.println("used: "+memoryUsed+" of "+os.ramSize+"("+(memoryUsed*100/os.ramSize).toString+"%)")
    output.println("free: "+freeMemory+" of "+os.ramSize+"("+(freeMemory*100/os.ramSize).toString+"%)")
    output.println("frame #\t\t\towner\t\t\tvalues")
    os.memory.zipWithIndex.foreach(frame => {
        val frameNumber:Int = frame._2
        val frameValues:scala.collection.mutable.Buffer[Int] = frame._1
        val frameOwner = os.pageOwners(frameNumber)
        output.println(frameNumber.toString+"\t\t\t"+frameOwner.toString+"\t\t\t"+frameValues.mkString(","))
    })

    output.println("Swap: ")
    val swapUsed = os.swapOwners.filter(_> (-1)).size
    val freeSwap = os.swapOwners.filter(_ < 0).size
    output.println("used: "+swapUsed+" of "+os.ramSize+"("+(swapUsed*100/os.ramSize)+"%)")
    output.println("free: "+freeSwap+" of "+os.ramSize+"("+(freeSwap*100/os.ramSize)+"%)")
    output.println("frame #\t\t\towner\t\t\tvalues")
    os.swap.zipWithIndex.foreach(swapFrame => {
        val frameNumber:Int = swapFrame._2
        val frameValues:scala.collection.mutable.Buffer[Int] = swapFrame._1
        val frameOwner = os.swapOwners(frameNumber)
        output.println(frameNumber.toString+"\t\t\t"+frameOwner.toString+"\t\t\t"+frameValues.mkString(","))
    })
  }
}
