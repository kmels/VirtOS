package core

import exceptions.{permissionToFileDeniedException,invalidHandlerIdException}

class IOInterface(os:OperatingSystem){
    val ioLog = new util.Log(os.pathToLogs+"io.log")
    ioLog.info("I/O log initialized")

  var files = scala.collection.mutable.Map[Int,java.io.BufferedReader]() //file handler id -> (taskId of the owner,FileHandler object)
  var fileOwners = scala.collection.mutable.Map[Int,Int]() //file handler id -> task owner id
  var fileNames = scala.collection.mutable.Map[Int,String]() //file handler id -> fileName
  
  def getNewFileHandler = files.size+1

  def openFile(taskId:Int,fileName:String):Int = {
    val pathToFile = os.shell.currentPath.path + fileName // <-- chapuz
    val handlerValue = getNewFileHandler

    val fileObject = new java.io.BufferedReader(new java.io.FileReader(pathToFile))
    
    ioLog.info("task "+os.tasks.get(taskId).get+" has opened \""+fileName+"\", handler assigned :"+handlerValue.toString)
    files.put(handlerValue,fileObject)
    fileOwners.put(handlerValue,taskId)
    fileNames.put(handlerValue,fileName)

    //yield handler Value
    handlerValue
  }

  def isFileOpen(fileName:String):Boolean ={
    val fileIterator = fileNames.valuesIterator

    //filter the files knowing the lead term of the tuple is the path to the file
    val filteredFile = fileIterator.filter(_==fileName)

    if (filteredFile.hasNext)
      //file is indeed open
      true
    else
      false
  }

  def isFileInUseByAnotherTask(fileName:String,askingTaskId:Int):Boolean = {
    var fileIsBeingUsedByAnotherTask = false

    if (isFileOpen(fileName)){
      //file is indeed open, check the owner now
      val handlersIterator = fileNames.keySet.iterator

      while (handlersIterator.hasNext){
        val nextHandlerId = handlersIterator.next
        val fileOwner = fileOwners.get(nextHandlerId).get


        if (fileNames.get(nextHandlerId).get==fileName)
          if (fileOwner != askingTaskId)
            fileIsBeingUsedByAnotherTask = true
      }
    }

    fileIsBeingUsedByAnotherTask
  }

def closeFile(taskId:Int,handlerId:Int) {
    if (files.contains(handlerId)){
      val fileToClose = files.get(handlerId).get
      val fileTaskOwner = fileOwners.get(handlerId).get
      val fileName = fileNames.get(handlerId).get

      if (fileTaskOwner == taskId){
        fileToClose.close
        //remove the file
        files -= handlerId
        fileOwners -= handlerId
        fileNames -= handlerId
        ioLog.info("task "+fileTaskOwner+" has closed the file with handler id "+handlerId.toString+" named:"+fileName)
      }else
        throw new permissionToFileDeniedException(os.tasks.get(taskId).get,os.tasks.get(fileTaskOwner).get,handlerId)
    } else
      throw new invalidHandlerIdException(handlerId)
  }

def readLineFromFile(taskId:Int,handlerId:Int) = {
    if (files.contains(handlerId)){
      val fileTaskOwner = fileOwners.get(handlerId).get

      if (fileTaskOwner == taskId){
        val fileObject = files.get(handlerId).get
        var readLine = fileObject.readLine

        ioLog.info("taskId "+taskId.toString+" has read a line from the file with handler id "+handlerId.toString)
        readLine //return the read line
      }
      else
        throw new permissionToFileDeniedException(os.tasks.get(taskId).get,os.tasks.get(fileTaskOwner).get,handlerId)
    } else
      throw new invalidHandlerIdException(handlerId)
  }
}
