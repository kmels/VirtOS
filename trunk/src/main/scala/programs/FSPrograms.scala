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
        val listOfFiles:List[FileControlBlock] = os.fs.getDirectoryFiles(p).sortBy(_.getName)
        listOfFiles.foreach(fcb => output.println(
          if (fcb.isDirectory)
            appendSlash(fcb.getName)
          else
            fcb.getName
        ))
      }
      case homePath(p) => {
        val absoluteHomePathFile = new File(os.pathToHome+p)
        if (absoluteHomePathFile.isDirectory)
          absoluteHomePathFile.listFiles.foreach(file => output.println(
            if (file.isDirectory)
              appendSlash(file.getName)
            else 
              file.getName
          ))
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
  
  def exec:Unit = output.println(path match {
    case fsPath(localPath) => localPath
    case homePath(pathToLocal) => "@/"+pathToLocal
  }) 
}

class mkdir(os:OperatingSystem,newDirPath:Path,outputObject:outputMethod) extends system_program{
  val programName = "mkdir"
  val number_of_max_params = 1
  val output = outputObject

  val pathComponents:Array[String] = newDirPath.path.split('/')
  val pathToParent:String = pathComponents.slice(0,pathComponents.size-1).mkString("/") match{
    case "" => ""
    case somePathToParent => appendSlash(somePathToParent)
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
    os.fs.fat.table.foreach(println _)
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
      case homePath(relativePath) => {
        val absoluteHostPath = os.pathToHome+relativePath
        if (!new File(absoluteHostPath).exists)
          throw new internalFSException("directory doesn't exist")
        else
          os.shell.setCurrentPath(new homePath(relativePath))
      }
    }
  } catch {
    case e => output.println(e.toString)
  }
}

class du(os:OperatingSystem,specifiedPath:Path,outputObject:outputMethod) extends system_program{
  val programName = "du"
  val number_of_max_params =0
  val output = outputObject

  val fsSizeInBytes = os.fs.getUserSpaceSize
  val freeSpaceInBytes = fsSizeInBytes - os.fs.getSpaceUsedInBytes
  
  //bytes to kilo
  def toKB(bytes:Int):Float = bytes/1024

  def round(f:Float):Float = Math.round(f*1000)/1000

  def exec:Unit = {
     output.println("Total space on disk: \t\t\t"+fsSizeInBytes+"Bytes \t "+round(toKB(fsSizeInBytes))+"KB")
     specifiedPath match {
       case fsPath(path) => {
         //relative to path
         val realSpaceUsedInPath = os.fs.getSpaceInBytesUsedIn(path)
         val realSizePercentageOfTotal:Float = realSpaceUsedInPath*100/fsSizeInBytes
         output.println("Real space used on specified path: \t"+realSpaceUsedInPath+" Bytes \t "+round(toKB(realSpaceUsedInPath))+" KB \t("+round(realSizePercentageOfTotal)+"% of total)")
         
         val spaceOnDiskUsedInPath = os.fs.getFileSizeOnDisk(path)
         val sizeOnDiskPercentageOfTotal:Float = spaceOnDiskUsedInPath*100/fsSizeInBytes
         output.println("Space on disk used on specified path: \t"+spaceOnDiskUsedInPath+" Bytes \t"+round(toKB(spaceOnDiskUsedInPath))+" KB \t("+round(sizeOnDiskPercentageOfTotal)+"% of total)")
         
         val wastedSpaceInPath = spaceOnDiskUsedInPath - realSpaceUsedInPath
         output.println("Wasted space on specified path: \t"+wastedSpaceInPath+" Bytes \t"+round(toKB(wastedSpaceInPath))+" KB.")

         //totally
         val totalSpaceOnDiskUsed = os.fs.getFileSizeOnDisk("~/")
         val totalSpaceOnDiskPercentageOfTotal:Float = totalSpaceOnDiskUsed*100/fsSizeInBytes
         val freeSpaceOnDisk = fsSizeInBytes-totalSpaceOnDiskUsed
         val percentageOfFreeSpaceOnDisk:Float = freeSpaceOnDisk*100/fsSizeInBytes
         output.println("Total space used on disk: \t\t"+totalSpaceOnDiskUsed+" Bytes \t"+round(toKB(totalSpaceOnDiskUsed))+"KB \t("+totalSpaceOnDiskPercentageOfTotal+"%)")
         output.println("Free space on disk: \t\t\t "+freeSpaceOnDisk+" Bytes \t"+round(toKB(freeSpaceOnDisk))+"KB \t("+percentageOfFreeSpaceOnDisk+"%)")
         //wasted space
       }
       case _ => output.println("Not yet implemented")
     } 
  }
}
class cp(os:OperatingSystem,absoluteSourcePath:Path,absoluteDestinyPath:Path,outputObject:outputMethod) extends system_program{
  val programName = "cp"
  val number_of_max_params = 1
  val output = outputObject
  
  /**
   * copies a file in local file system of content srcBytes in path
   */
  private def copyFileInFS(srcBytes:Array[Byte],localPath:fsPath):Unit = 
    os.fs.placeNewFile(localPath.path,srcBytes)

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
      val sourceDirFCB = os.fs.getFCBFromAbsolutePath(absoluteLocalPathToSourceDir)

      //make destiny dir first, so we can copy files/dirs inside
      new mkdir(os,destinyPath,output).exec
      val filesToCopy:List[FileControlBlock] = os.fs.getDirectoryFiles(absoluteLocalPathToSourceDir) 
      
      filesToCopy.foreach(fileFCB => {
        val localSourcePath:fsPath = os.fs.getPathToFCB(fileFCB)
        if (fileFCB.isDirectory) {
          //make new directory in destiny
          destinyPath match {
            case fsPath(localPathToDestiny) => copyDir(localSourcePath,new fsPath(appendSlash(localPathToDestiny)+fileFCB.getName))
            case homePath(homePathToDestiny) => copyDir(localSourcePath,new homePath(appendSlash(homePathToDestiny)+fileFCB.getName))
          }
        }else{
          //copy file, 
          val sourceBytes = os.fs.getFileContents(localSourcePath.path)
          //where to copy?
          destinyPath match{
            case fsPath(absoluteLocalPathToDestinyDir) => copyFileInFS(sourceBytes,new fsPath(appendSlash(absoluteLocalPathToDestinyDir)+fileFCB.getName))
            case homePath(absoluteHomePathToDestinyDir) => copyFileInHome(sourceBytes,new homePath(appendSlash(absoluteHomePathToDestinyDir)+fileFCB.getName))
          }
        }
      })
    }
    case homePath(homePathToSourceDir) =>{
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
            case fsPath(localPathToDestiny) => copyDir(new homePath(homePathToSourceDir),new fsPath(appendSlash(localPathToDestiny)+file.getName))
            case homePath(homePathToDestiny) => copyDir(new homePath(homePathToSourceDir),new homePath(appendSlash(homePathToDestiny)+file.getName))
          }
        } else{
          //get file bytes
          val sourceBytes = new Array[Byte](file.length.toInt)
          new RandomAccessFile(file,"r").readFully(sourceBytes)
          //where to copy?
          destinyPath match{
            case fsPath(absoluteLocalPathToDestinyDir) => copyFileInFS(sourceBytes,new fsPath(appendSlash(absoluteLocalPathToDestinyDir)+file.getName))
            case homePath(absoluteHomePathToDestinyDir) => copyFileInHome(sourceBytes,new homePath(appendSlash(absoluteHomePathToDestinyDir)+file.getName))
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
        if (!new File(os.pathToHome+path).exists)
          throw new internalFSException(path+" file: doesn't exist")

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

class rm(os:OperatingSystem,filePath:Path,outputObject:outputMethod) extends system_program {
  val programName = "rm"
  val number_of_max_params = 1
  val output = outputObject

  def exec:Unit = filePath match {
    case homePath(pathToFile) => {}
    case fsPath(pathToFile) => os.fs.removeFileOrDirectory(pathToFile)
  }
} 


class mv(os:OperatingSystem,sourcePath:Path,destinyPath:Path,outputObject:outputMethod) extends system_program{
  val programName = "mv"
  val number_of_max_params = 2
  val output = outputObject

  //when both source and destiny paths are not fsPath (we can't 'move')
  def copyAndRemove:Unit = {
    new cp(os,sourcePath,destinyPath,outputObject)
    new rm(os,sourcePath,outputObject)
  } 

  def exec:Unit = sourcePath match{
    case fsPath(pathToSource) => {
      destinyPath match {
        case fsPath(pathToDestiny) => os.fs.moveFileOrDirectory(pathToSource,pathToDestiny)
        case homePath(pathToDestiny) => copyAndRemove
      }
    }
    case homePath(pathToSource) => copyAndRemove
  }
}
/*class mount(os:OperatingSystem,pathToFS:Path,mountPath:Path,outputObject:outputMethod) extends system_program{
  val programName = "mount"
  val number_of_max_params = 2
  val output = outputObject

  val mountedFS = new FileSystem()
  def exec:Unit = try{
  }
}*/
