package programs
import core.OperatingSystem
import exceptions.invalidTaskIdentifier

class kill(os:OperatingSystem,taskId:Int,outputObject:core.outputMethod) extends system_program{
	val programName = "ps"
	val number_of_max_params = 1
	val output = outputObject
	val taskIdToKill = taskId
 
	def exec = {
          try
             os.scheduler.killTask(taskId)
          catch {
            case taskDoesntExistException:invalidTaskIdentifier => println(taskDoesntExistException.toString)
          }
        }
}
