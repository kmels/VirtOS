package exceptions

case class permissionToFileDeniedException(taskAsker:core.task,taskOwner:core.task,handlerId:Int) extends Exception{
  override def toString() = "Permission denied to task \""+taskAsker.toString+"\"; file with handler "+handlerId.toString+" is in use by task "+taskOwner.toString 
}