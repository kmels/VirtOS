package programs

import java.io.{File,RandomAccessFile}
import core.{OperatingSystem,FileControlBlock,outputMethod}
import exceptions.internalFSException
import util.FS._

class ls(os:OperatingSystem,path:Path,outputObject:outputMethod) extends system_program {
  val programName = "ls"
  val number_of_max_params = 0
  val output = outputObject

  def exec():Unit = try {
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
  } catch{
    case e => output.println(e.toString)
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

class du(os:OperatingSystem,outputObject:outputMethod) extends system_program{
  val programName = "du"
  val number_of_max_params =0
  val output = outputObject

  def exec:Unit = {
    /*val fsSizeInBytes = os.fs.getSizeInBytes
     val freeSpaceInBytes = os.fs.getFreeSpaceInBytes
     val freeSpaceInKB:Float = freeSpace/1024
     val fsSizeInKB:Float = fsSizeInBytes/1024
     output.println("Free space: "+freeSpaceInBytes+" bytes => "+freeSpaceInKB+"KB")
     output.println("Total space: "+fsSizeInBytes+" bytes => "+fsSizeInKB+"KB")*/
  }
}
class cp(os:OperatingSystem,absoluteSourcePath:Path,absoluteDestinyPath:Path,outputObject:outputMethod) extends system_program{
  val programName = "cp"
  val number_of_max_params = 1
  val output = outputObject
  
  def copyFileInFS(srcBytes:Array[Byte],path:String):Unit = {
    os.fs.placeNewFile(path,srcBytes)
  }

  def copyFileInHome(srcBytes:Array[Byte],absolutePath:String):Unit = {
    val file:RandomAccessFile = new RandomAccessFile(new File(absolutePath),"w")
    file.write(srcBytes) 
  }

  def exec:Unit = absoluteSourcePath match{
    case fsPath(path) => {
      //source is in this FS
      val sourceFCB:FileControlBlock = os.fs.getFCBFromAbsolutePath(path) match{
        case Some(fcb) => fcb
        case _ => throw new internalFSException("file/directory not found in localfile system: "+path)
      }
      
      if (sourceFCB.isFile) {
        //get bytes
        val sourceBytes:Array[Byte] = os.fs.getFileContents(path)

        //check destiny
        absoluteDestinyPath match{
          case fsPath(pathToDestiny) => copyFileInFS(sourceBytes,pathToDestiny)
          case homePath(pathToDestiny) => copyFileInHome(sourceBytes,pathToDestiny)
        }
      } else{
        //copy directory
      }
    }
    case homePath(path) => {
      //source is in home FS
      val homeFile = new File(path)
      if (!homeFile.exists)
        throw new internalFSException("home file doesn't exist: "+path)

      if (!homeFile.isDirectory){
        //get file bytes
        val file:RandomAccessFile = new RandomAccessFile(os.pathToHome+path,"r")
        val sourceBytes = new Array[Byte](file.length.toInt)
        file.readFully(sourceBytes)

        //check destiny
        absoluteDestinyPath match{
          case fsPath(pathToDestiny) => copyFileInFS(sourceBytes,pathToDestiny)
          case homePath(pathToDestiny) => copyFileInHome(sourceBytes,pathToDestiny)
        }
      }else {
        //copy directory
      }
    }
  } //end exec 
}

class cat(os:OperatingSystem,absolutePathToFile:Path,outputObject:outputMethod) extends system_program {
  val programName = "cat"
  val number_of_max_params = 1
  val output = outputObject

  def exec:Unit = try {
    val fileBytes:Array[Byte] = absolutePathToFile match{
      case fsPath(path) => os.fs.getFileContents(path)
      case homePath(path) => {    
        if (new File(os.pathToHome+path).isDirectory)
          throw new internalFSException(path+ " is not a file but a directory")

        val file:RandomAccessFile = new RandomAccessFile(os.pathToHome+path,"r")
        val sourceBytes = new Array[Byte](file.length.toInt)
        file.readFully(sourceBytes)
        sourceBytes
      }
    }
    output.print(fileBytes.map(_.toChar).mkString) 
  } catch {
    case e => output.println(e.toString)
  }
}
