def genPng = {
    import java.io.{BufferedReader,InputStreamReader,PrintStream,FileOutputStream,BufferedOutputStream}
    val pathToFile = "/Users/kmels/Desarrollo/scala/netbeans-workspace/kmelsOS/rootdir/stats/prueba.png"
    
    //val fileWriter = new BufferedOutputStream(new FileOutputStream(pathToFile))
    val output = new PrintStream(new FileOutputStream(pathToFile))

    val url = "http://chart.apis.google.com/chart?cht=p3&chd=t:0.21333334,0.14666666,0.64&chs=500x300&chl=Queue2|Queue1|Queue3&chtt=Execution%20time%20by%20Queues"

    import java.net.URL
    val pag = new URL(url).openConnection()

    val in = new BufferedReader(
                                  new InputStreamReader(
                                  pag.getInputStream()))
				  
    import scala.io.Source

    val s:Source = Source.fromURL(new URL(url))


    while (s.hasNext){
    	  output.print(s.next)
    }
    /*for (filteredLine <- s.getLines().toList){
    	Console.println(filteredLine)
	output.print(filteredLine)
    }*/

    Console.println("termino")
    
}