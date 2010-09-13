package programs
import core.OperatingSystem

class ps(os:OperatingSystem,outputObject:core.outputMethod) extends system_program{
  val programName = "ps"
  val number_of_max_params = 0
  val output = outputObject

  def exec = {
    output.print("Active tasks: \n")
    os.scheduler.queues.foreach(queue=>{
      val activeTasks = queue
      activeTasks.Queue.foreach (task => output.print(task.toString +"\n"))
    }
    )
  }
}
