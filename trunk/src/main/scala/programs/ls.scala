package programs

import java.io.File
import core.OperatingSystem

class ls(os:OperatingSystem,pathToDirectory:String,outputObject:core.outputMethod) extends system_program {
    val programName = "ls"
    val number_of_max_params = 0
    val directory = new File(os.pathToRoot)
    val output = outputObject

    def exec() {
       directory.listFiles() foreach {
         file => output.echo(file.getName + "\n")
       }
    }
}
