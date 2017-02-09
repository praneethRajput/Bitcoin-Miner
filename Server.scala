package com.dos.bitcoin
import akka.actor._
import com.typesafe.config.ConfigFactory

import java.io.File
import java.security.MessageDigest
import akka.routing.RoundRobinRouter

case class mineCoins(leadingZeroCount: Int, stringLength: Int)
case class Success(key: String, value: String)

object HelloRemote extends App  {

	if(!args.isEmpty && !args(0).isEmpty()){

		var leadingZeroCount = args(0).toInt
				println("--------------- Leading Zero Count provided " + leadingZeroCount + "--------------")

				val configFile = getClass.getClassLoader.getResource("Remoteapplication.conf").getFile
				val config = ConfigFactory.parseFile(new File(configFile))
				val system = ActorSystem("BitcoinMinerSystem",config)
				val remoteActor = system.actorOf(Props(new RemoteActor(leadingZeroCount)), name = "RemoteActor")
        
        remoteActor ! "StartMining"

	}else{
		println("Provide leading zeroes count to proceed with the computation")
	}

}

class RemoteActor(leadingZeroCount: Int) extends Actor {

	var workerCount = 4
			var workUnits = 25
			val workerRouter = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(workerCount)), name = "workerRouter")

			def receive = {
			case "RequestClientWork" => 
      println("Client has requested for work")
			sender ! String.valueOf(leadingZeroCount)
      
      case "StartMining" =>
      for(i <- 1 to workerCount){
        workerRouter ! mineCoins(leadingZeroCount, 10 + i)
      } 
      
      case Success(key, value) =>
      println("%s\t%s".format(key,value))
      
			case msg: String =>
			println(msg)
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
					sb.append("prajput+asirivella")
					for (i <- 1 to length) {
						sb.append(r.nextPrintableChar)
					}
			sb.toString()              
	}


}