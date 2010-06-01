package core

import java.io.{File,RandomAccessFile}
import util.FS._
import scala.collection.mutable.ArrayBuffer
import exceptions.internalFSException

class FileSystem(pathToFile:String){
   val emptyFCB = new FileControlBlock(-1,"",0,-1,0,0,'a',-1,-1)
  /**
   * Returns a new and clean, empty filesystem, with directory root ~/ created.
  */ 
  def getNewCleanFS(fsFile:File):RandomAccessFile = {
    println("Creating new file system")
    val newFS = new RandomAccessFile(fsFile,"rw")
       
    //write root directory
    val rootFCB = new FileControlBlock(0,"~",0,-1,now,now,'c',-1,-1)

    newFS.write(rootFCB.buffer)

    //write empty directory of FCBs
    assertEquals("FileSystem.mount, emptyFCB size",emptyFCB.getSize,0)
    for (i<-0 to 82) {
      newFS.write(emptyFCB.buffer)
    }
    
    assert(newFS.length==256*84) //assert on the directory size,21KB

    // Create File Allocation Table 
    for(block <- 0 to 999){
      newFS.write(whitespace) //byte 0
      newFS.write('-'.toByte) //byte 1 
      newFS.write(byteFromDigit(1)) //byte 2
    }

    //This space is wasted, 
    newFS.write(emptySequence(72)) // FAT size should be 3KB exactly (72 bytes are lost)
    
    assertEquals("FileSystem.munt, FCBDir + FAT size",newFS.length,256*84+1024*3) //21KB + 4KB of the FAT

    val emptyDataBlock = emptySequence(1000*1024)
    newFS.write(emptyDataBlock)
    assertEquals("getNewCleanFS size",newFS.length,1024*1024)
    newFS
  }

  /**
   * Try to mount
   */
   def mount(fsFile:File):RandomAccessFile = 
     if (fsFile.exists){
       //just load it
       new RandomAccessFile(fsFile,"rw")
     }else{
       getNewCleanFS(fsFile)
     }

  private val fsFile:RandomAccessFile = mount(new File(pathToFile))
  
  /**
   * Load the directory
   */  
  require(fsFile.length == 1024*1024,"Invalid FS: Its length should be 1MB")
  println("Creating FCB directory")
  val fcbDirectory = new Directory(fsFile)
  println("Creating FAT")
  val fat = new FAT(fsFile)
  println("Creating Data")
  val data = new Data(fsFile)
  
  /**
   * Creates a new directory where "path" specifies, named "name"
   */
  def placeDirectory(path:String,name:String):Unit = {
    val fcb:FileControlBlock = fcbDirectory.placeNewDirectory(path,name)
  }

  def getDirectoryFiles(absolutePath:String):List[FileControlBlock] = fcbDirectory.getDirectoryContents(absolutePath)    

  def getDirectoryFCB(absolutePath:String):Option[FileControlBlock] = {
    fcbDirectory.getFCBFromAbsolutePath(absolutePath) match {
      case Some(fcb) => {
        if (fcb.isDirectory)
          Some(fcb)
        else
          throw new internalFSException("FCB is a file, not a directory")
      }
      case _ => None
    }
  }

  /**
   * returns the path to the given FCB
   */
  def getPathToFCB(fcb:FileControlBlock) = {
    fcbDirectory.getPathToFCB(fcb)
  }

  /**
   * Creates a new file where "path" specifies, with contents given by sourceBytes
  */ 
  def placeNewFile(pathToFile:String,content:Array[Byte]):Unit = {
    val pathComponents = pathToFile.split('/')
    val pathToDir:String = pathComponents.slice(0,pathComponents.size-1).mkString("/")
    val fileName = pathComponents.slice(pathComponents.size-1,pathComponents.size).mkString
    val fileSize = content.size
    val allocations:List[FileAllocation] = fat.allocate(content.size)

    allocations match{
      case List() => throw new internalFSException("not enough allocations")
      case List(firstAllocation,_*) => {
        fcbDirectory.placeNewFile(pathToDir,fileName,fileSize,firstAllocation.blockId)
        val blocksToUpdate:List[Int] = allocations.map(_.blockId)
        data.updateBlocksWith(blocksToUpdate,content)
      }
    }    
  }

  def getFCBFromAbsolutePath(path:String) = fcbDirectory.getFCBFromAbsolutePath(path)

  /**
   * returns the content of a file
   * @throws internalFSException if path is invalid
   * @throws internalFSException if specified path is a directory
   */ 
  def getFileContents(path:String):Array[Byte] = {
    val fcb:FileControlBlock = fcbDirectory.getFCBFromAbsolutePath(path) match{
      case Some(fileControlBlock) => fileControlBlock
      case _ => throw new internalFSException("invalid Path: "+path)
    }
    
    if (!fcb.isFile)
      throw new internalFSException("Not a file: "+path)
      
    val blocks = getFileBlockIds(fcb)
    
    val extendedContent = blocks.foldLeft(Array[Byte]())((a,b) => a++data.get(b))
    //yield content only, until EOF, that is the size of the file
    val EOFIndex:Int = fcb.getSize
    extendedContent.slice(0,EOFIndex)   
  }

  /**
   * returns the blocks of a given path to file
   */
  private def getFileBlockIds(fcb:FileControlBlock):List[Int] = {
    if (!fcb.isFile)
      throw new internalFSException("Not a file: "+getPathToFCB(fcb))
      
    val firstBlock = fcb.getFirstBlock
    fat.getAllocationsFrom(firstBlock)
  }

  /**
   * returns the size on disk of a given file or directory
   */
  def getFileSizeOnDisk(absolutePath:String):Int = {
    val fcb:FileControlBlock = fcbDirectory.getFCBFromAbsolutePath(absolutePath) match{
      case Some(fileControlBlock) => fileControlBlock
      case _ => throw new internalFSException("invalid Path: "+absolutePath)
    }

    fcb.isFile match {
      case true => getFileBlockIds(fcb).size*1024
      case _ => {
        val fileSizesOnDisk:List[Int] = getDirectoryFiles(absolutePath).map(fcb =>{
          if (fcb.isFile)
            getFileBlockIds(fcb).size*1024
          else
            getFileSizeOnDisk(getPathToFCB(fcb).path)
        })

        //sum the file sizes
        fileSizesOnDisk.size match{
          case 0 => 0
          case _ => fileSizesOnDisk.reduceLeft(_+_)
        }        
      }
    }
  }

  /**
   * measures space in bytes for a given path, and returns its value. If it's a directory, it returns the sum of its children size recursively.
   */
  def getSpaceInBytesUsedIn(absolutePath:String):Int = {
    val fcb:FileControlBlock = fcbDirectory.getFCBFromAbsolutePath(absolutePath) match{
      case Some(fileControlBlock) => fileControlBlock
      case _ => throw new internalFSException("invalid Path: "+absolutePath)
    }

    fcb.isFile match {
      case true => fcb.getSize
      case _ => {
        val fileSizes:List[Int] = getDirectoryFiles(absolutePath).map(fcb =>{
          if (fcb.isFile)
            fcb.getSize
          else
            getSpaceInBytesUsedIn(getPathToFCB(fcb).path)
        })
        //sum the file sizes
        fileSizes.size match{
          case 0 => 0
          case _ => fileSizes.reduceLeft(_+_)
        }        
      }
    }
  }

  /**
   * Returns the space used in this fs.
   */
  def getSpaceUsedInBytes:Int = {
    val usedFileSizes:List[Int] = fcbDirectory.FCBs.filter(_.getId  > -1).toList.map(_.getSize)
    usedFileSizes.reduceLeft(_+_)
  }

  def getUserSpaceSize:Int = 1024*1000 //1000 KB

  /**
   *Removes a file or a directory, if pathtoFile is a directory, it deletes it's files first.
   */ 
  def removeFileOrDirectory(pathToFile:String):Unit = getFCBFromAbsolutePath(pathToFile) match {
    case Some(file) =>  file.isDirectory match {
      case true => {
        //check that it's not root directory
        if (file.getId==0)
          throw internalFSException("Can't delete directory ~/")

        //delete files first
        getDirectoryFiles(getPathToFCB(file).path).foreach(fcb => removeFileOrDirectory(getPathToFCB(fcb).path))
        //delete dir
        fcbDirectory.removeFCB(file.getId)
      }
      case _ => fcbDirectory.removeFCB(file.getId)
    }
    case _ => throw internalFSException("file doesnt exist")
  }
} //end class FileSystem
