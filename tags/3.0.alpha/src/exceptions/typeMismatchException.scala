package exceptions

case class typeMismatchException(val message:String) extends Exception{
	override def toString() = "Type mismatch: "+message
}