/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package programs
import core.sys.stats

class statsgen(outputObject:core.outputMethod) extends system_program{
  val programName = "statsgen"
  val number_of_max_params = 0
  val output = outputObject

  def exec() = {
    core.sys.stats.generate(core.shell.currentDir+"stats/",output)
  }
}