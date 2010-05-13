package util

class Log(pathToFile:String) {
	val output = new java.io.PrintStream(new java.io.FileOutputStream(pathToFile))

        def apply(message:String) = info(message)
        
	def info(message:String) {
	  output.print("("+System.nanoTime+") INF "+message+"\n")
	  //Console.println(message+"\n")
	}
}
