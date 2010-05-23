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

class mkdir(os:OperatingSystem,newDirPath:Path,outputObject:outputMethod) extends system_program{
  println("making dir: "+newDirPath)

  val programName = "mkdir"
  val number_of_max_params = 1
  val output = outputObject

  val pathComponents:Array[String] = newDirPath.path.split('/')
  val pathToParent:String = pathComponents.slice(0,pathComponents.size-1).mkString("/") match{
    case "" => ""
    case somePathToParent => somePathToParent+"/"
  }
  val newDirName:String = pathComponents.last

  def exec:Unit = newDirPath match {
    case fsPath(pathToNewDir) => os.fs.placeDirectory(pathToParent,newDirName) 
    case homePath(pathToNewDir) => new File(os.pathToHome+pathToParent+newDirName).mkdirs
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
  
  /**
   * copies a file in local file system of content srcBytes in path
   */
  private def copyFileInFS(srcBytes:Array[Byte],localPath:fsPath):Unit = {
    println("copiando file en local FS con path: "+localPath.path)
    os.fs.placeNewFile(localPath.path,srcBytes)
  }

  /**
   * analog to copyFileINFS but in home fs
   */
  private def copyFileInHome(srcBytes:Array[Byte],homePath:homePath):Unit = {
    val absoluteHostPath:String = os.pathToHome+homePath.path
    println("copiando file en host FS con path: "+absoluteHostPath)
    val file:RandomAccessFile = new RandomAccessFile(new File(absoluteHostPath),"rw")
    file.write(srcBytes) 
  }

  /**
   * copies a dir recursively from sourcePath to destinyPath (both could be either local or from host FS)
   */
  private def copyDir(sourcePath:Path,destinyPath:Path):Unit = sourcePath match{
    case fsPath(absoluteLocalPathToSourceDir) => {
    }
    case homePath(homePathToSourceDir) =>{
      println("copiando de :"+homePathToSourceDir+" a "+destinyPath.path)
      val absoluteHomePathToSourceDir = os.pathToHome + homePathToSourceDir
      val sourceDirFile = new File(absoluteHomePathToSourceDir)
      
      if (!sourceDirFile.exists)
        throw new internalFSException("source directory doesn't exist: "+homePathToSourceDir)

      //make dir first, so we can copy files/dirs inside 
      new mkdir(os,destinyPath,output).exec
      val filesToCopy:Array[File] = sourceDirFile.listFiles
      
      filesToCopy.foreach( file =>{
        if (file.isDirectory){
          //get path relative to os.pathToHome
          val homePathToSourceDir = file.getAbsolutePath.slice(os.pathToHome.size,file.getAbsolutePath.size)
          destinyPath match {
            case fsPath(localPathToDestiny) => copyDir(new homePath(homePathToSourceDir),new fsPath(localPathToDestiny+"/"+file.getName))
            case homePath(homePathToDestiny) => copyDir(new homePath(homePathToSourceDir),new homePath(homePathToDestiny+"/"+file.getName))
          }
        } else{
          //get file bytes
          val sourceBytes = new Array[Byte](file.length.toInt)
          new RandomAccessFile(file,"r").readFully(sourceBytes)
          //where to copy?
          destinyPath match{
            case fsPath(absoluteLocalPathToDestinyDir) => copyFileInFS(sourceBytes,new fsPath(absoluteLocalPathToDestinyDir+"/"+file.getName))
            case homePath(absoluteHomePathToDestinyDir) => copyFileInHome(sourceBytes,new homePath(absoluteHomePathToDestinyDir+"/"+file.getName))
          }
        }
      })
    }
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
          case fsPath(pathToDestiny) => copyFileInFS(sourceBytes,new fsPath(pathToDestiny))
          case homePath(pathToDestiny) => copyFileInHome(sourceBytes,new homePath(pathToDestiny))
        }
      } else
        //copy directory
        copyDir(absoluteSourcePath,absoluteDestinyPath)
    }
    case homePath(path) => {
      //source is in home FS
      val homeFile = new File(os.pathToHome+path)
      if (!homeFile.exists)
        throw new internalFSException("home file doesn't exist: "+path)

      if (!homeFile.isDirectory){
        //get file bytes
        val file:RandomAccessFile = new RandomAccessFile(os.pathToHome+path,"r")
        val sourceBytes = new Array[Byte](file.length.toInt)
        file.readFully(sourceBytes)

        //check destiny
        absoluteDestinyPath match{
          case fsPath(pathToDestiny) => copyFileInFS(sourceBytes,new fsPath(pathToDestiny))
          case homePath(pathToDestiny) => copyFileInHome(sourceBytes,new homePath(pathToDestiny))
        }
      }else 
        //copy directory, source: home FS
        copyDir(absoluteSourcePath,absoluteDestinyPath)
    } //end exec 
  }
} //end cp

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
