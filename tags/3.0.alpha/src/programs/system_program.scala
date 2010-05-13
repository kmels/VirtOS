package programs

abstract class system_program() extends program{
  val programName:String
  val number_of_max_params : Int
  def exec()
  //system program are not scheduled
  val burstTime = 0
}
