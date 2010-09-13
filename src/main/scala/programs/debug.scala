package programs

import core.OperatingSystem

class debug(os:OperatingSystem,entry:List[String],outputObject:core.outputMethod) extends system_program{
	val programName = "debug"
	val number_of_max_params = 0
	val output = outputObject
	val entryToDebug = entry
 
  def exec() = {
    Console.println("Debug va a ejecutar: "+entry.mkString(" "))
    val programMeta = os.shell.getTaskMeta(entry.mkString(" "))
    
    programMeta match {
      case Some(taskMeta) => os.shell.exec(0,taskMeta,true)
      case _ => {}
    }
  }
}
