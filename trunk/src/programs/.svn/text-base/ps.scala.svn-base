package programs
import core.sys

class ps(outputObject:core.outputMethod) extends system_program{
  val programName = "ps"
  val number_of_max_params = 0
  val output = outputObject

  def exec = {
    output.print("Active tasks: \n")
    sys.scheduler.queues.foreach(queue=>{
      val activeTasks = queue
      activeTasks.Queue.foreach (task => output.print(task.toString +"\n"))
    }
    )
  }
}
