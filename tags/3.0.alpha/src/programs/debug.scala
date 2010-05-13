package programs

class debug(entry:List[String],outputObject:core.outputMethod) extends system_program{
	val programName = "debug"
	val number_of_max_params = 0
	val output = outputObject
	val entryToDebug = entry
 
  def exec() = {
    Console.println("Debug va a ejecutar: "+entry.mkString(" "))
    val programMeta = core.shell.getProgramFromEntry(entry.mkString(" "))
    core.shell.exec(0,programMeta,true)
  }
}
