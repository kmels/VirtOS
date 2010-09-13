package exceptions

case class unknownCommandException(val commandName:String) extends Exception{
	override def toString = "ksh: "+commandName+": command not found"
}
