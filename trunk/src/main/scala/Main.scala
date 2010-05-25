package core   
  
object Main extends Application{
  override def main(args:Array[String]){
    new OperatingSystem(System.getProperty("user.dir")+"/sys.xml")
  }
}
