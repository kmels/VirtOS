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
  def setSize(newFileSize:Int):Unit = applyPatch(34,stringToByteArray(extendToRightString(newFileSize.toString,7)),7)

  setSize(size)

  //set first block, -1 means an empty dir
  def setFirstBlock(firstBlock:Int):Unit = {
    applyPatch(2+32+7,stringToByteArray(extendToRightString(firstBlock.toString,4)),4)
  }
  setFirstBlock(firstBlock)

  //set creation and modification date
  applyPatch(2+32+7+4,stringToByteArray(extendToRightString(creationDate.toString,32)),32)

  def setModificationDate(timestamp:Long):Unit = applyPatch(2+32+7+4+32,stringToByteArray(extendToRightString(timestamp.toString,32)),32)
  
  setModificationDate(modificationDate)

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
   * places a new file, returns it's new fcb
   */ 
  def placeNewFile(absolutePath:String,name:String,size:Int,firstBlock:Int):FileControlBlock = getFCBFromAbsolutePath(absolutePath) match {
    case Some(parentFCB) => {
      if (!parentFCB.isDirectory)
        throw new internalFSException("can't create file in "+absolutePath+", it's not a directory")

      //look for a free fcb
      FCBs.zipWithIndex.find(_._1.getId <0) match {
        case Some(fcbZippedWithIndex) => {
          val freeFCBIndex = fcbZippedWithIndex._2
          //update parents last child sibling
          parentFCB.getFirstBlock match {
            case -1 => { //the directory has no children
              FCBs(parentFCB.getId).setFirstBlock(freeFCBIndex) 
              flushFCBId(parentFCB.getId)
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
          
          //place the new file
          FCBs(freeFCBIndex) = new FileControlBlock(freeFCBIndex,name,size,firstBlock,now,now,'a',parentFCB.getId,-1)
          flushFCBId(freeFCBIndex)
          FCBs(freeFCBIndex)
        }
        case _ => throw internalFSException("Not enough space for a new FCB")
      }
    }
    case _ => throw new internalFSException("Can't find FCB of "+absolutePath)
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
  } //end getPathToFCB


  /**
   * removes a file or a directory, ignoring it's new FCB, just updates it's parents and siblings, just like "kill fcb"
   */
  def removeFCBAndUpdateSiblings(fcbId:Int):Unit = { 
    //fix parents or sibling
    if (FCBs(fcbId).getParentId < 0)
      throw internalFSException("Can't delete root directory")

    val parent:FileControlBlock = FCBs(FCBs(fcbId).getParentId)
    assert(parent.isDirectory) //it should!

    val isFirstChild:Boolean = parent.getFirstBlock==fcbId
    val hasYoungerSibling = FCBs(fcbId).getSiblingId>0

    if (!isFirstChild){ //update siblings      
       assert(FCBs.exists(_.getSiblingId==fcbId))  //assert on the existence on the older sibling
        val olderSibling:FileControlBlock =  FCBs.find(_.getSiblingId==fcbId).get //this has to exist since we asserted on its existence before
      
      if (hasYoungerSibling){ //update older to have it's sibling as fcbIds younger sibling
        val youngerSiblingId = FCBs(fcbId).getSiblingId
        FCBs(olderSibling.getId).setSiblingId(youngerSiblingId)
      }else 
        FCBs(olderSibling.getId).setSiblingId(-1)      

        flushFCBId(olderSibling.getId)
    }else {      
      if (!hasYoungerSibling) //is only child
        //delete parents reference
        FCBs(parent.getId).setFirstBlock(-1)
       else //it has some younger sibling, update parent
        FCBs(parent.getId).setFirstBlock(FCBs(fcbId).getSiblingId)

      flushFCBId(parent.getId)
    }
  }

  /**
   * cleans a FCB
   */ 
  def cleanFCB(fcbId:Int):Unit = {
    removeFCBAndUpdateSiblings(fcbId)
    //finaly "delete" it
    FCBs(fcbId) = new FileControlBlock(-1,"",0,-1,0,0,'a',-1,-1)
    flushFCBId(fcbId)
  }
  /**
   *moves FCB from sourcePath to destinyPath
   */
  def moveFCB(sourceFCBId:Int,futureParentId:Int):Unit = {
    removeFCBAndUpdateSiblings(sourceFCBId)
    FCBs.slice(0,3) foreach println

    //make the parent have the source FCB as a new child
    FCBs(futureParentId).getFirstBlock match {
      case -1 => { //parent has no children
        FCBs(futureParentId).setFirstBlock(sourceFCBId)
        flushFCBId(futureParentId)
      }
      case _ => { //parent has at least 1 child
        val lastSibling:FileControlBlock = getFCBChildren(FCBs(sourceFCBId)).last
        FCBs(lastSibling.getId).setSiblingId(sourceFCBId)
        flushFCBId(lastSibling.getId)
      }
    }

    FCBs(sourceFCBId).setParentId(futureParentId)
    flushFCBId(sourceFCBId)
  }

  /**
   * Changes a FCB name
   */
  def changeFCBName(fcbId:Int,newFileName:String):Unit = {
    FCBs(fcbId).setName(newFileName)
    flushFCBId(fcbId)
  }

  /**
   * sets a new file size
   */
  def setFileSize(fcbId:Int,newSize:Int):Unit = {
    FCBs(fcbId).setSize(newSize)
    flushFCBId(fcbId)
  }

} //end class Directory

/**
 * A FAT record: it's value is:
 * 1. None if it's end of file
 * 2. -1 if Free
 * 3. # of block following (from 0 to 999)
 */ 
sealed case class FileAllocation(blockId:Int,value:Option[Int]){
  def isEndOfFile = value match { //wether it's the end of a file
    case Some(value) => false
    case _ => true //it's None (sequence of 3 whitespaces)
  }
  def isFree:Boolean = if (!isEndOfFile) value.get < 0 else false//if it's -1
  def nextBlockId:Option[Int] = if (!isEndOfFile) value else None

  override def toString:String = "index: "+blockId+", next: "+nextBlockId
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

  } yield fileAllocationFromBuffer(fileAllocation_i,fileAllocationBuffer)

  /**
   * Allocates blocks for a new File 
   */
  def allocate(numberOfBytesToAllocate:Int):List[FileAllocation] =  {
    val freeAllocations:Array[(FileAllocation,Int)] = table.zipWithIndex.filter(_._1.isFree)
    val netBlocksNeeded = numberOfBytesToAllocate / 1024
    val blockOffset = if (numberOfBytesToAllocate%1024==0) 0 else 1
    val blocksNeeded = netBlocksNeeded + blockOffset match {
      case 0 => 1 //allocate at least 1
      case neededBlocks => neededBlocks
    }

    if (freeAllocations.size<blocksNeeded)
      throw new internalFSException("not enough space in FAT")

    val assignedAllocations = freeAllocations.slice(0,blocksNeeded)
    assert(assignedAllocations.size==blocksNeeded)
    

    val allocations:List[FileAllocation] = assignedAllocations.zipWithIndex.toList.map(zippedAssignedAllocation => {
      val indexOfAssignedAllocation = zippedAssignedAllocation._2
      val assignedAllocation = zippedAssignedAllocation._1
      val assignedBlockIndex:Int = assignedAllocation._2
      if (indexOfAssignedAllocation==blocksNeeded-1){
        //it's the last block 
        table(assignedBlockIndex) = new FileAllocation(assignedBlockIndex,None)
        flushFileAllocation(assignedBlockIndex)
      }
      else {
        val nextAssignedBlockIndex = assignedBlockIndex+1
        table(assignedBlockIndex) = new FileAllocation(assignedBlockIndex,Some(nextAssignedBlockIndex))
        table(assignedBlockIndex)
        flushFileAllocation(assignedBlockIndex)
      }      
    })
    allocations
  }
/**
   * get all allocations from
   */
  def getAllocationsFrom(blockId:Int):List[Int] =  table(blockId).nextBlockId match{
    case Some(nextBlock) => blockId::getAllocationsFrom(nextBlock)
    case _ => List(blockId)
  }

  def flushFileAllocation(blockIndex:Int):FileAllocation = {
    val allocationIndexInMappedBuffer = blockIndex*3
    val allocationData:Array[Byte] = table(blockIndex).nextBlockId match {
      case Some(block) => {

        stringToByteArray(extendToRightString(block.toString,3))
      }
      case _ => ArrayBuffer.fill(3)(32.toByte).toArray //white spaces
    }

    for (i <- 0 to 2)
      mappedBuffer.put(allocationIndexInMappedBuffer+i,allocationData(i))
    table(blockIndex)
  }
} //end FAT

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

  def get(block:Int) = blocks(block).data

  /**
   * writes to mapped buffer
   */
  def flushBlock(blockId:Int):Block = {
    val blockIndexInMappedBuffer = blockId*1024
    val blockData = blocks(blockId).data

    for (i <- 0 to 1023)
      mappedBuffer.put(blockIndexInMappedBuffer+i,blockData(i))
    blocks(blockId)
  }

  def updateBlocksWith(blocksToUpdate:List[Int],content:Array[Byte]):Unit ={
    //validation for internal use
    val netBlocksNeededForContent = content.size/1024
    val offsetOfBlocksNeeded = content.size%1024 match{
      case 0 => 0
      case _ => 1
    }
    val blocksNeeded = netBlocksNeededForContent+offsetOfBlocksNeeded match {
      case 0 => 1 //at least 1 is needed
      case neededBlocks => neededBlocks
    }

    assert(blocksNeeded==blocksToUpdate.size)

    //extend content, e.g. fill wasted bytes with -1 (simulating an EOF)
    val endIndexOfNetBytes = netBlocksNeededForContent*1024
    val numberOfBytesToExtend = 1024-content.size%1024//bytes to fill with -1
    val extendedBytes:Array[Byte] = ArrayBuffer.fill(numberOfBytesToExtend)(EOF).toArray
    val extendedContent:Array[Byte] = content.slice(0,endIndexOfNetBytes)++content.slice(endIndexOfNetBytes,endIndexOfNetBytes+content.size%1024)++extendedBytes

    assert(extendedContent.size%1024==0) //it has to fit exactly in blocks of 1024 bytes

    //parse data blocks from extended content
    val dataBlocks:List[Block] = for { 
      blockIndex <- (0 to blocksNeeded-1).toList
      val blockData = extendedContent.slice(blockIndex*1024,blockIndex*1024+1024)
    } yield new Block(blockData)

    blocksToUpdate.zipWithIndex.foreach(zippedBlock => {
      val blockToUpdateID = zippedBlock._1
      val indexOfUpdatingBlock = zippedBlock._2
      blocks(blockToUpdateID) = dataBlocks(indexOfUpdatingBlock)
      flushBlock(blockToUpdateID)
    })
  }

  /**
   * appends content to a block
   */
  def appendContentTo(blockId:Int,indexToStartAppending:Int,data:Array[Byte]):Unit = {
    if (indexToStartAppending>1023)
      throw new internalFSException("Cant append, blocks are of size 1024")

    val blockData = get(blockId)    
    for (i <- 0 until data.size)
      blockData(i+indexToStartAppending) = data(i)

    blocks(blockId) = new Block(blockData)
    flushBlock(blockId)
  }
}
