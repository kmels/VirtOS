package core

import util.FS._
import exceptions.internalFSException

class FileSystemFile(fs:FileSystem,filePath:fsPath) {
  val pathToFile = filePath.path
  val fcb:FileControlBlock = fs.getFCBFromAbsolutePath(pathToFile) match {
    case Some(fileControlBlock) => fileControlBlock
    case _ => throw new internalFSException("no such file")
  }
  
  def print(m:String):Unit = fs.appendContentsToFile(fcb,stringToByteArray(m)) 
}
