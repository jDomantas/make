package domantas.make

import scala.io.Source;

object Parser {
	def parse(makefile: String): Either[String, Makefile] = {
		var lines = Source.fromFile(makefile).getLines().toSeq.dropWhile(_.trim.isEmpty)
		var rules: List[Rule] = List()
		
		while (!lines.isEmpty) {
			parseRule(lines) match {
				case Left(err) => return Left(err)
				case Right((rule, rest)) => 
					rules = rule :: rules
					lines = rest.dropWhile(_.trim.isEmpty)
			}
		}
		
		Right(new Makefile(rules.reverse))
	}
	
	private def parseRule(lines: Seq[String]): Either[String, (Rule, Seq[String])] = {
		assert(!lines.isEmpty)
		
		val head = lines.head
		if (!head.contains(":"))
			return Left(s"invalid rule start: '$head'")
			
		val colon = head.indexOf(":")
		val target = head.substring(0, colon).trim
		val prerequisites = head.substring(colon + 1)
			.split(" ")
			.map(_.trim)
			.filter(_.length > 0)
			.toSeq
			
		val recipe = lines.tail
			.takeWhile(_.startsWith("\t"))
			.map(_.substring(1))
			.toSeq
		
		if (recipe.isEmpty)
			return Left(s"no recipe for: '$head'")
		
		Right((new Rule(target, prerequisites, recipe), lines.drop(recipe.length + 1)))
	}
}