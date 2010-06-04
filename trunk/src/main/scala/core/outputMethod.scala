package core
import util.FS._
import java.io.{PrintStream,FileOutputStream}

trait outputReference{
  val outputFile:AnyRef

  def print(m:String):Unit = outputFile match{
    case hostFile:PrintStream => hostFile.print(m)
    case localFile:FileSystemFile => localFile.print(m)
    case console => Console.print(m)
  }

  def flush = outputFile match{
    case hostFile:PrintStream => hostFile.close
    case _ => {}
  }
}

class outputMethod(os:OperatingSystem,isAsynchrounous:Boolean,filePath:Path) extends outputReference{

  val outputIsAsynchronous = isAsynchrounous
  
  val outputFile:AnyRef = outputIsAsynchronous match{
    case true => filePath match{
      case homePath(pathToFile) => new java.io.PrintStream(new java.io.FileOutputStream(os.pathToHome+pathToFile))
      case fsPath(pathToFile) => os.fs.getFCBFromAbsolutePath(pathToFile) match{
        case Some(fcb) => {
          new FileSystemFile(os.fs,new fsPath(pathToFile))
        }
        case _ => {
          println("no existe")
          os.fs.placeNewFile(pathToFile,Array.fill(0)(0)) //create new file
        }
      }
    }
    case _ => Console
  }
  
/*  def print(message:String){
    if (!outputIsAsynchronous)
      os.shell.print(message)
    else
      outputFile.print(message)
  }*/
  
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

  def println(message:String) = print(message+"\n")  
}
