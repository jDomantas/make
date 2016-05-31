package domantas.make

import scala.sys.process.Process
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class BuildNode(
		val target: String, 
		val recipe: Seq[String], 
		val children: Seq[BuildNode]) {
	
	// result is created eagerly, so the program
	// will process the maximum possible set of items
	val result: Future[Seq[String]] = buildNode(children.map(_.result))

	def buildNode(items: Seq[Future[Seq[String]]]): Future[Seq[String]] = {
		for (results <- Future.sequence(items))
			yield {
				val flattened = results.flatten
				if (flattened.isEmpty) 
					executeRecipe()
				else 
					flattened
			}
	}
	
	def executeRecipe(): Seq[String] = {
		// find first error, or return None if everything went fine
		recipe.view.flatMap(command => {
			val process = Process(createCommand(command))
			println(command)
			val exitCode = process.!
			if (exitCode != 0)
				Some(s"'$command' exited with code $exitCode when building '$target'")
			else
				None
		}).headOption match {
			case None => Seq()
			case Some(e) => Seq(e)
		}
	}
	
	private def createCommand(buildStep: String): Seq[String] = {
		// just use "cmd" on windows and "sh" on everything else
		sys.props("os.name").toLowerCase match {
			case x if x contains "windows" => Seq("cmd", "/C", buildStep)
			case _ => Seq("sh", "-c", buildStep)
		}
	}
}