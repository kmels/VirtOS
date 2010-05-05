package exceptions

case class unknownStateIdException(val message:String) extends Exception{
  override def toString() = "Internal error: "+message
}

