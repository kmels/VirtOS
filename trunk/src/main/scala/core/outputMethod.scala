package core

class outputMethod(os:OperatingSystem,isAsynchrounous:Boolean,pathToFile:String) {
  val outputIsAsynchronous = isAsynchrounous
  
  val outputFile:java.io.PrintStream = if (outputIsAsynchronous)
    new java.io.PrintStream(new java.io.FileOutputStream(pathToFile)) 
                                       else
                                         null
  
  def print(message:String){
    if (!outputIsAsynchronous)
      os.shell.print(message)
    else
      outputFile.print(message)
  }
  
  def echo(message:String){
    //echo either a register value or a string	  
    if (os.isRegister(message))
      //echo register value
      print(os.getRegisterValue(message).toString)
    else{
      if (message.contains("\\n"))
        print(message.replaceAll("\\\\n","\n"))
      else
        print(message)
    }
  }

  def println(message:String) = 
    if (!outputIsAsynchronous)
      os.shell.println(message)
    else
      outputFile.println(message)
  
  override def toString = { "pathToFile: "+pathToFile}
  
  def flush = if (outputIsAsynchronous) outputFile.close
}
