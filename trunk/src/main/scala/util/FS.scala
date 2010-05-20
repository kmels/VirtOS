package util

import scala.collection.mutable.ArrayBuffer
import java.util.Calendar
import core.{FileControlBlock,FileAllocation}
import exceptions.internalFSException

object FS{
  trait Path{val path:String}
  case class fsPath(val path:String) extends Path
  case class homePath(val path:String) extends Path

  val whitespace:Byte = 32

  def emptySequence(size:Int):Array[Byte] = ArrayBuffer.fill(size)(whitespace).toArray

  def now:Long = Calendar.getInstance().getTime.getTime

  def stringToByteArray(s:String):Array[Byte] = s.toArray.map(_.toByte)

  def byteArrayToString(a:Array[Byte]) = a.foldLeft("")(_+_.toChar.toString)

  def byteToString(b:Byte):String = b.toChar.toString

  def assertEquals(m:String,s1:AnyVal,s2:AnyVal) = if (s1!=s2) println("Failed in "+m+", should be: "+s2+", got "+s1)

  def byteFromDigit(d:Int):Byte = d.toString.charAt(0).toByte //Receives a digit and return it's string representation byte e.g. Char 56 would return Byte 8

  def extendToRightString(s:String,requestedSize:Int):String = {
    assert(s.size<=requestedSize)
    var string = s
    for (missing<-1 to requestedSize-s.size)
      string += " "
    assertEquals("extendToRightString",string.size,requestedSize)
    string
  }

  def fileControlBlockFromBuffer(buffer:Array[Byte]):FileControlBlock = {
    assertEquals("fileControlBlockFromBuffer",buffer.size,256)
    
    val id:Int = byteArrayToString(buffer.slice(0,2)).toInt
    val name:String = byteArrayToString(buffer.slice(2,2+32).toArray)
    assertEquals("fileControlBlockFromBuffer: name.size",name.size,32)
    val size:Int = byteArrayToString(buffer.slice(2+32,2+32+7)).trim.toInt
    val firstBlock:Int = byteArrayToString(buffer.slice(2+32+7,2+32+7+4)).trim.toInt
    val creationDate:Long = byteArrayToString(buffer.slice(2+32+7+4,2+32+7+4+32)).trim.toLong
    val modificationDate:Long = byteArrayToString(buffer.slice(2+32+7+4+32,2+32+7+4+32+32)).trim.toLong
    val fileOrDir:Char = buffer(2+32+7+4+32+32).toChar

    val parentId:Int =  byteArrayToString(buffer.slice(2+32+7+4+32+32+1,2+32+7+4+32+32+1+2)).trim.toInt
    
    val siblingId:Int = byteArrayToString(buffer.slice(2+32+7+4+32+32+1+2,2+32+7+4+32+32+1+2+2)).trim.toInt

    new FileControlBlock(id,name,size,firstBlock,creationDate,modificationDate,fileOrDir,parentId,siblingId)
  }

  def fileAllocationFromBuffer(buffer:Array[Byte]):FileAllocation = {
    require(buffer.size==3,"fileAllocationFromBuffer size==3")

    byteArrayToString(buffer).trim match {
      case "" =>  new FileAllocation(None) //end of File
      case possibleIntValue => try{
        val intValue:Int = possibleIntValue.toInt
        new FileAllocation(Some(intValue))
      } catch {
        case invalidFileAllocation:NumberFormatException => throw new internalFSException("invalid file allocation value found: "+byteArrayToString(buffer))
      }
    }
  }
}


