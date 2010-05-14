package exceptions

case class invalidLabelException(labelName:String,line:Int) extends Exception{
  override def toString = "Label "+labelName+" is invalid, either it doesn't exist or has an invalid name at line "+line
}