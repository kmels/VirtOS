import core.OperatingSystem
  
object Main extends Application {

  val configurationFile:String = System.getProperty("user.dir")+"/sys.xml"

  val kmelsOS = new OperatingSystem(configurationFile)

}
