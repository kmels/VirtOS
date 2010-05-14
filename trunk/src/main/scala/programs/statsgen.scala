/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package programs
import core.OperatingSystem
import util.Statistics

class statsgen(os:OperatingSystem,outputObject:core.outputMethod) extends system_program{
  val programName = "statsgen"
  val number_of_max_params = 0
  val output = outputObject

  def exec() = {
    new Statistics(os).generate(os.pathToRoot+"stats/",output)
  }
}
