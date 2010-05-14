package exceptions

import core.Task

case class permissionToFileDeniedException(taskAsker:Task,taskOwner:Task,handlerId:Int) extends Exception{
  override def toString() = "Permission denied to task \""+taskAsker.toString+"\"; file with handler "+handlerId.toString+" is in use by task "+taskOwner.toString 
}
