package programs
import core.{OperatingSystem,Task}

class ps(os:OperatingSystem,outputObject:core.outputMethod) extends system_program{
  val programName = "ps"
  val number_of_max_params = 0
  val output = outputObject

  def exec = {
    val activeTasks:List[Task] = os.tasks.filter(task => task._2.state == 2 || task._2.state ==3).map(_._2).toList
    
    activeTasks.foreach(task => output.print(task.toString+"\n"))
  }
}
