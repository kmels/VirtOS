package exceptions

case class frameTooLargeException(taskId:Int,numberOfRequestedFrames:Int,frameSize:Int) extends Exception{
  override def toString = "task "+taskId.toString+" asked for "+numberOfRequestedFrames+" and frame size is "+frameSize
}

