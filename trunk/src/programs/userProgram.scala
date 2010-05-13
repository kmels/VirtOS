package programs

import java.io.File
import core.sys
import exceptions._

class userProgram(fileName:File, outputObject:core.outputMethod) extends program {
    val programName = fileName.getName
    //val outputIsAsynchronous = isAsynchronous
    //val pathToOutputFile = pathToFile

    val output = outputObject


    //parse lines
    var parsedLines: List[(Int,List[String])] = Nil

    for (filteredLine <- scala.io.Source.fromFile(fileName, 1000000).getLines().toList.zipWithIndex){
        val lineValue = filteredLine._1

        val parsedLine:List[String] =
        if (lineValue.filter(chr => chr == '\"').size==2){
                //then, parse the text inside the quotes as one element
                val firstIndex = lineValue.indexOf("\"")
                val lastIndex = lineValue.lastIndexOf("\"")
                val quotedText = lineValue.substring(firstIndex+1,lastIndex)
                val firstSlice = lineValue.slice(0,firstIndex).trim()
                val lastSlice = lineValue.slice(lastIndex+1,lineValue.length).trim()

                val tail = if (lastSlice.length!=0)
                        List(quotedText) ::: lastSlice.split(' ').toList
                else
                        List(quotedText)

                firstSlice.split(' ').toList ::: tail
        } else{
          //simple parse by spaces
          if (lineValue.trim().length!=0)
                  lineValue.trim().split(' ').toList
  else
      Nil
        }

        if ((parsedLine!=Nil) && (!lineValue.startsWith(";"))){
                val lineNumber = filteredLine._2 + 1
                val appendingList = List(lineNumber -> parsedLine)
                parsedLines = parsedLines ::: appendingList
        }
  }
  
  val labels =  scala.collection.mutable.Map[String,Int]()

  def getLabelName(label:String) = label.substring(1,label.length-1).toUpperCase

  //parse labels
  for (line <- parsedLines){
             val lineNumber = line._1
             val lineValue = line._2
             try {
                     lineValue match {
                     case List(possibleLabel:String) =>
                         if ((possibleLabel.startsWith("'")) && (possibleLabel.endsWith("'")))
                              //then it's a label, check if it's not contained yet, add, else, throw an executionException
                            if (!labels.contains(possibleLabel))
                              labels.put(getLabelName(possibleLabel),lineNumber)
                            else
                                    throw new labelAlreadyDeclared(possibleLabel,labels.get(possibleLabel).get)
                     case _ =>  //do nothing (it's not a label)
                     }
             }
             catch{
                   case twoLabelsSameName : exceptions.labelAlreadyDeclared  => throw new executionException("Execution error at line "+lineNumber+": "+twoLabelsSameName.toString)
             }
  }


  val burstTime = sys.getBurstTime(this)
}
