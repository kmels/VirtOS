package programs

import java.io.File
import core.{OperatingSystem,FileControlBlock,outputMethod}
import exceptions.internalFSException
import util.FS._

class ls(os:OperatingSystem,path:Path,outputObject:outputMethod) extends system_program {
    val programName = "ls"
    val number_of_max_params = 0
    val output = outputObject

    def exec():Unit = {
      path match {
        case fsPath(p) => {
          val listOfFiles:List[FileControlBlock] = os.fs.getDirectoryContents(p)
          listOfFiles.foreach(fcb => output.println(fcb.getName))
        }
        case homePath(p) => {
          val absoluteHomePathFile = new File(os.pathToHome+p)
          if (absoluteHomePathFile.isDirectory)
            absoluteHomePathFile.listFiles.foreach(file => output.println(file.getName))
          else
            throw internalFSException(absoluteHomePathFile.getName+" is not a directory")
        }
      }
    }
}

class pwd(path:Path,outputObject:outputMethod) extends system_program{
  val programName = "pwd"
  val number_of_max_params = 0
  val output = outputObject
  
  def exec:Unit ={
    output.println(path.path)
  }
}

class mkdir(os:OperatingSystem,currentPath:Path,dirName:String,outputObject:outputMethod) extends system_program{
  val programName = "mkdir"
  val number_of_max_params = 1
  val output = outputObject

  def exec:Unit = currentPath match {
    case fsPath(p) => os.fs.placeDirectory(p,dirName) 
    case homePath(p) => new File(os.pathToHome+p+"/"+dirName).mkdirs
  }
}

class lsFCB(os:OperatingSystem,outputObject:outputMethod) extends system_program{
  val programName = "lsFCB"
  val number_of_max_params = 0
  val output = outputObject

  def exec:Unit = {
    os.fs.fcbDirectory.FCBs.foreach(println _)
  }
}

class cd(os:OperatingSystem,path:Path,outputObject:outputMethod) extends system_program{
  val programName = "cd"
  val number_of_max_params = 1
  val output = outputObject

  def exec:Unit = try {
    path match{
      case fsPath(absolutePath) => {
        os.fs.getDirectoryFCB(absolutePath) match {
          case Some(fcb) => os.shell.setCurrentPath(os.fs.getPathToFCB(fcb))
          case _ => throw new internalFSException("directory doesn't exist")
        }}
      case homePath(relativePath) => {} //to do
    }
  } catch {
    case e => output.println(e.toString)
  }
}

/*class cp(os:OperatingSystem,absoluteSourcePath:String,absoluteDestinyPath:String,outputObject:outputMethod) extends system_program{
}
*/
