
import akka.actor._
import com.typesafe.config.ConfigFactory
import java.io.File
import java.security.MessageDigest
import akka.routing.RoundRobinRouter

case class mineCoins(leadingZeroCount: Int, stringLength: Int)
case class Success(key: String, value: String)
object Local extends App {

	if(!args.isEmpty && !args(0).isEmpty()){

		println("-----------String Bitcoin Mining Client ---------------------")

		val configFile = getClass.getClassLoader.getResource("localapplication.conf").getFile
		val config = ConfigFactory.parseFile(new File(configFile))
		val system = ActorSystem("ClientSystem",config)
		val localActor = system.actorOf(Props(new LocalActor(args(0))), name="local")
		localActor ! "RequestWork"                                                     


	}else{
		println("Server IP address needed to start client")
	}


}

class LocalActor(hostAddress: String) extends Actor {

	// create the remote actor
	val remote = context.actorFor("akka.tcp://BitcoinMinerSystem@" + hostAddress +":5150/user/RemoteActor")
			var workerCount = 4
			var workUnits = 25
			val workerRouter = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(workerCount)), name = "workerRouter")

			def receive = {
			case "RequestWork" => 
			remote ! "RequestClientWork"

			case msg: String => 
			var leadingZeroCount = msg.toInt
			for(i <- 1 to workerCount){
				workerRouter ! mineCoins(leadingZeroCount, 10 + i)
			}          

			case Success(key, value) =>
			remote ! "%s\t%s".format(key,value)

	}
}

class Worker extends Actor {

	def receive = {

	case mineCoins(leadingZeroCount, stringLength) =>
	for(i <- 1 to 10000000)
	{
		var key = generateRandomStringOfLength(stringLength)
				var hashString = hex_digest(key)
				if(isBitCoin(hashString,generateTargetPattern(leadingZeroCount) )){
					sender ! Success(key, hashString)
				}
	}
	}
	def isBitCoin(hashString: String, targetPattern: String) : Boolean = {

			return (hashString.substring(0, targetPattern.length()) == targetPattern)
	}

	def generateTargetPattern(leadingZeroCount: Int): String = {

			var targetPattern = new StringBuilder()
			for(i <- 1 to leadingZeroCount){
				targetPattern.append("0")
			}
			return targetPattern.toString()
	}

	def hex_digest(s: String): String = {
			val sha = MessageDigest.getInstance("SHA-256")
					sha.digest(s.getBytes())
					.foldLeft("")((s: String, b: Byte) => s +
					Character.forDigit((b & 0xf0) >> 4, 16) +
					Character.forDigit(b & 0x0f, 16))
	}

	def generateRandomStringOfLength(length:Int): String ={
			val r = new scala.util.Random     
					val sb = new StringBuilder     
					sb.append("asirivella+prajput")
					for (i <- 1 to length) {
						sb.append(r.nextPrintableChar)
					}
			sb.toString()              
	}
}
