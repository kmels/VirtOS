package exceptions

case class invalidHandlerIdException(val handlerId:Int) extends Exception{
  override def toString = "Invalid Handler: "+handlerId.toString
}