package domantas.make

import java.io.File
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}

object Program {
	def main(args: Array[String]) {
		
		val file = new File("Makefile")
		if (!file.exists) {
			println("makefile not found")
			System.exit(1)
		}
		
		Parser.parse("Makefile") match {
			case Left(err) => 
				println(err)
				System.exit(1)
			case Right(makefile) =>
				val target = if (args.length > 0) 
						args(0) 
					else 
						makefile.defaultTarget.getOrElse {
							println("no target given and no default target")
							System.exit(1)
							return
						}
				
				val result = makefile.buildFile(target)
				
				val errors = Await.result(result, scala.concurrent.duration.Duration.Inf)
				if (!errors.isEmpty) {
					println("build failed:")
					for (err <- errors)
						println(err)
						
					System.exit(1)
				}
		}		
	}
}