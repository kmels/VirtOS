package core

import util.FS._
import scala.collection.mutable.ArrayBuffer
import exceptions.internalFSException
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel
import FileChannel.MapMode
import java.io.{RandomAccessFile}

class FileControlBlock(id:Int,name:String,size:Int,firstBlock:Int,creationDate:Long,modificationDate:Long,fileOrDir:Char,parentId:Int,siblingId:Int){
  //empty buffer
  val buffer:Array[Byte] = ArrayBuffer.fill(256)(whitespace).toArray //an empty FCB
  def applyPatch(from:Int,src:Array[Byte],replaced:Int) {
    for (i<-from until replaced+from)
      buffer(i) = src(i-from)
  }

  assertEquals("FileControlBlock",buffer.size,256)
  //set id
  assert(id<84)

  if (id < 0 ){
    buffer(0) = '-'.toByte
    buffer(1) = '1'.toByte
  }else{ 
    buffer(0) = byteFromDigit(id/10)
    buffer(1) = byteFromDigit(id%10)
  }

  //set name
  if (name.size>32)
    throw new internalFSException("names must be 31 characters long at most, size provided: "+name.size)

  def setName(name:String):Unit = applyPatch(2,stringToByteArray(extendToRightString(name,32)),32)
  setName(name)

  //set size
  applyPatch(34,stringToByteArray(extendToRightString(size.toString,7)),7)

  //set first block, -1 means an empty dir
  def setFirstBlock(firstBlock:Int):Unit = {
    applyPatch(2+32+7,stringToByteArray(extendToRightString(firstBlock.toString,4)),4)
  }
  
  setFirstBlock(firstBlock)

  //set creation and modification date
  applyPatch(2+32+7+4,stringToByteArray(extendToRightString(creationDate.toString,32)),32)
  applyPatch(2+32+7+4+32,stringToByteArray(extendToRightString(modificationDate.toString,32)),32)

  assert(fileOrDir=='a' || fileOrDir=='c')

  //file or directory
  buffer(2+32+7+4+32+32) = fileOrDir.toByte
  
  //set parent id
  def setParentId(parentId:Int):Unit = {
    applyPatch(2+32+7+4+32+32+1,stringToByteArray(extendToRightString(parentId.toString,2)),2)
  }

  setParentId(parentId)

  def setSiblingId(id:Int):Unit = {
    applyPatch(2+32+7+4+32+32+1+2,stringToByteArray(extendToRightString(id.toString,2)),2)
  }

  setSiblingId(siblingId)

  def getId:Int = byteArrayToString(buffer.slice(0,2)).toInt
  def getName:String = byteArrayToString(buffer.slice(2,2+32).toArray).trim
  def getSize:Int = byteArrayToString(buffer.slice(2+32,2+32+7)).trim.toInt
  def getFirstBlock:Int = byteArrayToString(buffer.slice(2+32+7,2+32+7+4)).trim.toInt
  def getCreationDate:Long = byteArrayToString(buffer.slice(2+32+7+4,2+32+7+4+32)).trim.toLong
  
  def getModificationDate:Long = byteArrayToString(buffer.slice(2+32+7+4+32,2+32+7+4+32+32)).trim.toLong
    
  def getFileOrDir:Char = buffer(2+32+7+4+32+32).toChar

  def getParentId:Int = byteArrayToString(buffer.slice(2+32+7+4+32+32+1,2+32+7+4+32+32+1+2)).trim.toInt
    
  def getSiblingId:Int = byteArrayToString(buffer.slice(2+32+7+4+32+32+1+2,2+32+7+4+32+32+1+2+2)).trim.toInt

  def isFile = getFileOrDir match{
    case 'a' => true
    case _ => false
  }

  def isDirectory = getFileOrDir match{
    case 'c' => true
    case _ => false
  }

  assertEquals("FileControlBlock",buffer.size,256)

  override def toString = "id: "+getId+",name: "+getName+",size: "+getSize+",first Block: "+getFirstBlock+",creation Date: "+getCreationDate+",modification Date: "+getModificationDate+", type: "+getFileOrDir+",parent: "+getParentId+", sibling: "+getSiblingId
}

case class Directory(file:RandomAccessFile) {
  val mappedBuffer:MappedByteBuffer = file.getChannel.map(MapMode.READ_WRITE,0,256*84) 
  
  val FCBs:Array[FileControlBlock] = for {
    fcb_i <- (0 to 83).toArray
    val fcbBuffer:Array[Byte] = for {
      fcb_i_pos <- (0 to 255).toArray
    } yield mappedBuffer.get(fcb_i*256+fcb_i_pos)
    val newFCB = fileControlBlockFromBuffer(fcbBuffer)
  } yield newFCB
  
  def rootDir:FileControlBlock = FCBs(0)

  assertEquals("Directory: FCBs.size",FCBs.size,84)


  /**
   * Returns a List[FileControlBlock] of the parameter "fcb" representing a directory
   */ 
  def getFCBChildren(fcb:FileControlBlock):List[FileControlBlock] = {
    if (!fcb.isDirectory)
      throw internalFSException("getFCBChildren: received a fcb File, needed: fcb Directory")

    FCBs.find(_.getId==fcb.getFirstBlock && fcb.getFirstBlock>0) match{
      case Some(child) => child::getFCBSiblings(child)
      case None => List()
    }
  }

  /**
   * Returns a List[FileControlBlock] of the parameter "fcb" reprensenting a file
   */

  def getFCBSiblings(fcb:FileControlBlock):List[FileControlBlock] ={
    FCBs.find(_.getId==fcb.getSiblingId && fcb.getSiblingId>0) match{
      case Some(sibling) => sibling::getFCBSiblings(sibling)
      case None => List()
    }
  }

  /**
   * Receives an absolute path and returns a list of the directory contents
   */
  def getDirectoryContents(absolutePath:String):List[FileControlBlock] = {
    getFCBFromAbsolutePath(absolutePath) match {
      case Some(fcb) => getFCBChildren(fcb) 
      case _ => throw internalFSException("invalid path: "+absolutePath)
    }
  }

  /**
   * Writes to mapped buffer
   */ 
  def flushFCBId(id:Int):FileControlBlock = {
    val fcbIndexInMappedBuffer = id*256
    val fcbBuffer = FCBs(id).buffer

    for (i <- 0 to 255)
      mappedBuffer.put(fcbIndexInMappedBuffer+i,fcbBuffer(i))
    FCBs(id)
  }

  /**
   * mkdir
   * Places a new directory in absolutePath, with name "name"
  */ 
  def placeNewDirectory(absolutePath:String,name:String):FileControlBlock = {
    val parentFCB:Option[FileControlBlock] =  getFCBFromAbsolutePath(absolutePath)
    parentFCB match {
      case Some(parent) => FCBs.zipWithIndex.find(_._1.getId<0) match{
        case Some(freeFCB) => {
          val freeFCBIndex = freeFCB._2
          //update parents tree
          parent.getFirstBlock match {
            case -1 => {
              FCBs(parent.getId).setFirstBlock(freeFCBIndex)
              flushFCBId(parent.getId)
            }
            case firstChild => {
              val siblings =  getFCBSiblings(FCBs(firstChild)) 

              val lastSibling:FileControlBlock = siblings match{
                case List() => FCBs(firstChild) //first Child is also the last
                case _ => siblings.last
              }

              //update last Sibling
              FCBs(lastSibling.getId).setSiblingId(freeFCBIndex)
              flushFCBId(lastSibling.getId)
            }
          }

          FCBs(freeFCBIndex) = new FileControlBlock(freeFCBIndex,name,0,-1,now,now,'c',parent.getId,-1)
          flushFCBId(freeFCBIndex)
          FCBs(freeFCBIndex)
        }
        case None => throw new internalFSException("No more File Control Blocks available")
      }
      case None => throw new internalFSException("Invalid Path to parent "+absolutePath) 
    }
  }

  /**
   * Returns an Option[FileControlBlock] if this path exists (wether a dir or file)
   * canonicalPath:String expresses the relative path from "parentFCB"
   * i.e. relativePath = "helloworld" and path(parentFCB)="~/" has an absolute of "~/helloworld"
   *
  * An absolute path can be called getFCBFromCanonicalPath(absolutePath,"")
  */
  def getFCBFromAbsolutePath(absolutePath:String):Option[FileControlBlock] = {
    def getFCBDescendentFrom(pathToDescendent:String,parentFCB:Option[FileControlBlock]):Option[FileControlBlock] = {
      parentFCB match {
        case Some(parent) => {
          val canonicalPathComponents = pathToDescendent.split('/')
          canonicalPathComponents match {
            case Array() => {Some(parent)}
            case Array(siblingName,_*) => {
              if (siblingName.trim == "")
                Some(parent)
              else {
                val siblingFCB = try {
                  getFCBChildren(parent).find(_.getName.trim==siblingName)
                } catch {
                  case e:internalFSException => throw new internalFSException(siblingName+" is not a directory")
                }
                val childrenOfSibling:Array[String] = canonicalPathComponents.tail //droped siblingName
                childrenOfSibling match {
                  case Array() => siblingFCB
                  case childrenCanonicalPathComponentsFromSibling => getFCBDescendentFrom(childrenCanonicalPathComponentsFromSibling.mkString("/"),siblingFCB)
                  
                }
              }
            } //end case Array(siblingName,_*)
          }
        }
        case _ => None
      } 
    }
      
    val absolutePathComponents = absolutePath.split('/')
    absolutePathComponents match {
        case Array("~",_*) => getFCBDescendentFrom(absolutePathComponents.tail.mkString("/"),Some(rootDir))
        case nel => println("path no absoluto: "+nel.mkString(",")) ; Some(new FileControlBlock(-1,"",0,-1,0,0,'a',-1,-1))
      }    
  } //end getFCBFromAbsolutePath 

  def getPathToFCB(fcb:FileControlBlock):fsPath = fcb.getParentId match{
    case -1 => if (fcb.getId==0) 
      new fsPath("~/")
    else 
      throw new internalFSException("can't find path to FCB")
    case parentId => new fsPath(getPathToFCB(FCBs(parentId)).path+fcb.getName+"/")
  }
} //end class Directory

/**
 * A FAT record: it's value is:
 * 1. None if it's end of file
 * 2. -1 if Free
 * 3. # of block following (from 0 to 999)
 */ 
sealed case class FileAllocation(value:Option[Int]){
  def isEndOfFile = value match { //wether it's the end of a file
    case Some(value) => false
    case _ => true //it's None (sequence of 3 whitespaces)
  }
  def isFree:Boolean = if (!isEndOfFile) value.get < 0 else false//if it's -1
  def nextBlockId:Option[Int] = if (!isEndOfFile) value else None
}

/**
 *File Allocation table
 */
case class FAT(file:RandomAccessFile){
  val mappedBuffer:MappedByteBuffer = file.getChannel.map(MapMode.READ_WRITE,256*84,3*1024)
  val table:Array[FileAllocation] = for {
    fileAllocation_i <- (0 to 1023).toArray

    val fileAllocationBuffer:Array[Byte] = for{
      fileAllocation_i_pos <- (0 to 2).toArray
    } yield mappedBuffer.get(fileAllocation_i*3+fileAllocation_i_pos)

  } yield fileAllocationFromBuffer(fileAllocationBuffer)
}

sealed case class Block(data:Array[Byte]){
  assert(data.size==1024)//1KB
}

case class Data(file:RandomAccessFile){
  val mappedBuffer:MappedByteBuffer = file.getChannel.map(MapMode.READ_WRITE,24*1024,1000*1024)
  
  val blocks:Array[Block] = for {
    block_i <- (0 to 999).toArray
    val blockBuffer:Array[Byte] = for {
      block_i_pos <- (0 to 1023).toArray
    } yield mappedBuffer.get(block_i*1024+block_i_pos)
  } yield new Block(blockBuffer)
}
