package util

import scala.collection.mutable.ArrayBuffer
import java.util.Calendar
import core.{FileControlBlock,FileAllocation}
import exceptions.internalFSException

object FS{
  trait Path{val path:String}
  case class fsPath(val path:String) extends Path
  case class homePath(val path:String) extends Path
  
  val emptyFCB =  new FileControlBlock(-1,"",0,-1,0,0,'a',-1,-1)
  val whitespace:Byte = 32
  val EOF:Byte = whitespace //for compatibility

  def appendSlash(s:String):String = if (s.endsWith("/") || s=="") s else s+"/"

  def emptySequence(size:Int):Array[Byte] = ArrayBuffer.fill(size)(whitespace).toArray

  def now:Long = Calendar.getInstance().getTime.getTime

  def stringToByteArray(s:String):Array[Byte] = s.toArray.map(_.toByte)

  def byteArrayToString(a:Array[Byte]) = a.foldLeft("")(_+_.toChar.toString)

  def byteToString(b:Byte):String = b.toChar.toString

  def assertEquals(m:String,s1:AnyVal,s2:AnyVal) = if (s1!=s2) println("Failed in "+m+", should be: "+s2+", got "+s1)

  def byteFromDigit(d:Int):Byte = d.toString.charAt(0).toByte //Receives a digit and return it's string representation byte e.g. Char 56 would return Byte 8

  def getParentFromPathToFile(p:String) = p match {
    case "~/" => "~/"
    case _ => appendSlash(p.split('/').init.mkString("/"))
  }

  def getFileNameFromPathToFile(p:String) = p.split('/').last

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
    
    try {
      //returns -1 if it's "" or an int conversion from a string, exception is not handled 
      def getIntInstanceFromString(s:String):Int = s.trim match {
        case "" => -1
        case someString => someString.toInt
      }

      def getLongInstanceFromString(s:String):Long = s.trim match {
        case "" => (-1).toLong
        case someString => someString.toLong
      }

      val id:Int = getIntInstanceFromString(byteArrayToString(buffer.slice(0,2)))
      val name:String = byteArrayToString(buffer.slice(2,2+32).toArray)
      assertEquals("fileControlBlockFromBuffer: name.size",name.size,32)
      val size:Int = getIntInstanceFromString(byteArrayToString(buffer.slice(2+32,2+32+7)))
      val firstBlock:Int = getIntInstanceFromString(byteArrayToString(buffer.slice(2+32+7,2+32+7+4)))
      val creationDate:Long = getLongInstanceFromString(byteArrayToString(buffer.slice(2+32+7+4,2+32+7+4+32)))
      val modificationDate:Long = getLongInstanceFromString(byteArrayToString(buffer.slice(2+32+7+4+32,2+32+7+4+32+32)))
      val fileOrDir:Char = buffer(2+32+7+4+32+32).toChar
      if (fileOrDir!='c' && fileOrDir!='a')
        throw new internalFSException("invalid char for file or directory in FCB: "+fileOrDir)

      val parentId:Int =  getIntInstanceFromString(byteArrayToString(buffer.slice(2+32+7+4+32+32+1,2+32+7+4+32+32+1+2)))
      
      val siblingId:Int = getIntInstanceFromString(byteArrayToString(buffer.slice(2+32+7+4+32+32+1+2,2+32+7+4+32+32+1+2+2)))
      new FileControlBlock(id,name,size,firstBlock,creationDate,modificationDate,fileOrDir,parentId,siblingId)
    } catch {
      case e:java.lang.NumberFormatException => throw new internalFSException("Could not load FCB from buffer: "+buffer.mkString(","))
    }  
  }

  def fileAllocationFromBuffer(id:Int,buffer:Array[Byte]):FileAllocation = {
    require(buffer.size==3,"fileAllocationFromBuffer size==3")

    byteArrayToString(buffer).trim match {
      case "" =>  new FileAllocation(id,None) //end of File
      case possibleIntValue => try{
        val intValue:Int = possibleIntValue.toInt
        new FileAllocation(id,Some(intValue))
      } catch {
        case invalidFileAllocation:NumberFormatException => throw new internalFSException("invalid file allocation value found: "+byteArrayToString(buffer))
      }
    }
  }
}
