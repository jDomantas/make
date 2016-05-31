package domantas.make

import java.io.File
import scala.concurrent.Future

class Makefile(val rules: Seq[Rule]) {
	
	def defaultTarget = rules.headOption.map(_.name)
	
	private def createBuildGraph(file: String, currentMap: Map[String, Option[BuildNode]]):
		Either[String, Map[String, Option[BuildNode]]] = {
		
		if (currentMap.contains(file))
			return Right(currentMap)
		
		val rule = findRule(file) match {
			case Some(r) => 
				r
			case None if new File(file).exists => 
				return Right(currentMap + (file -> None))
			case None => 
				return Left(s"no recipe to make '$file'")
		}
		
		var resultMap = currentMap
		for (item <- rule.prerequisites if !resultMap.contains(item)) {
			createBuildGraph(item, resultMap) match {
				case e @ Left(_) => return e
				case Right(map) => resultMap = map
			}
		}
		
		if (shouldRebuild(rule) || 
			rule.prerequisites.map(resultMap(_)).exists(_.isDefined)) {
			if (!resultMap.contains(file))
				resultMap = resultMap + (file -> Some(new BuildNode(
						rule.name,
						rule.recipe, 
						rule.prerequisites.flatMap(resultMap(_)))))
			else
				return Left(s"circular dependency from '$file'")
		} else {
			resultMap = resultMap + (file -> None)
		}
		
		Right(resultMap)
	}
	
	def buildFile(file: String): Future[Seq[String]] = {
		val rule = findRule(file) match {
			case Some(r) => 
				r
			case None if new File(file).exists => 
				return Future.successful(Seq(s"nothing to be done for '$file'"))
			case None => 
				return Future.successful(Seq(s"no recipe to make '$file'"))
		}
		
		return createBuildGraph(file, Map()) match {
			case Left(e) => Future.successful(Seq(e))
			case Right(map) => 
				map(file).map(_.result).getOrElse({
					println(s"'$file' is up to date")
					Future.successful(Seq())
				})
		}
	}
	
	def findRule(file: String): Option[Rule] = {
		// use view for lazy matching (stop on first found)
		rules.view.flatMap(_.matchFile(file)).headOption
	}
	
	private def shouldRebuild(rule: Rule): Boolean = {
		val target = new File(rule.name)
		!target.exists || 
		rule.prerequisites.view
			.map(new File(_))
			.exists(_.lastModified > target.lastModified)
	}
}