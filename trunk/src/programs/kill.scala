package programs
import core.{sys}
import exceptions.invalidTaskIdentifier

class kill(taskId:Int,outputObject:core.outputMethod) extends system_program{
	val programName = "ps"
	val number_of_max_params = 1
	val output = outputObject
	val taskIdToKill = taskId
 
	def exec = {
          try
             sys.scheduler.killTask(taskId)
          catch {
            case taskDoesntExistException:invalidTaskIdentifier => throw invalidTaskIdentifier(taskId," when attempting to kill it, it does not exist or has never existed.")
          }
        }
}
