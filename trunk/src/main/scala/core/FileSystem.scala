package core

import java.io.{File,RandomAccessFile}
import util.FS._
import scala.collection.mutable.ArrayBuffer
import exceptions.internalFSException

class FileSystem(pathToFile:String){
  val emptyFCB =  new FileControlBlock(-1,"",0,-1,0,0,'a',-1,-1)
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

  def getDirectoryContents(absolutePath:String):List[FileControlBlock] = fcbDirectory.getDirectoryContents(absolutePath)    

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
    val pathToDir = pathToFile.slice(0,pathComponents.size)
    val fileName = pathComponents.slice(pathComponents.size-1,pathComponents.size).mkString
    val fileSize = content.size
    
    val allocations:List[FileAllocation] = fat.allocate(content.size)
    
    println("allocations given: "+allocations.mkString(","))

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

  def getFileContents(path:String):Array[Byte] = {
    val fcb:FileControlBlock = fcbDirectory.getFCBFromAbsolutePath(path) match{
      case Some(fileControlBlock) => fileControlBlock
      case _ => throw new internalFSException("invalid Path: "+path)
    }
    
    if (!fcb.isFile)
      throw new internalFSException("Not a file: "+path)
      
    val firstBlock = fcb.getFirstBlock
    println("va a obtener allocatoins desde: "+firstBlock)
    val blocks = fat.getAllocationsFrom(firstBlock)
    
    val extendedContent = blocks.foldLeft(Array[Byte]())((a,b) => a++data.get(b))
    //yield content only, until EOF (-1)
    val EOFIndex:Int = extendedContent.indexOf(EOF) 
    
    if (EOFIndex<0)
      throw new internalFSException("file:"+path+" doens't have an EOF")

    extendedContent.slice(0,EOFIndex)   
  }
} //end class FileSystem
