package exceptions

case class internalFSException(val message:String) extends Exception{
  override def toString = message
}

