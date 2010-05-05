/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package exceptions

case class invalidTaskIdentifier(taskId:Int,message:String) extends Exception{
   override def toString = "invalidTaskIdentifier:" + taskId.toString + message
}
