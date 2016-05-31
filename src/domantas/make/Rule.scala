package domantas.make

class Rule(
		val name: String, 
		val prerequisites: Seq[String], 
		_recipe: Seq[String]) {
	
	val recipe = if (!name.contains("%")) {
		_recipe.map(expandAutomaticVariables(_))
	} else {
		_recipe
	}
	
	def matchFile(file: String): Option[Rule] = {
		if (name == file) { 
			return Some(this)
		} else {
			val stem = getStem(file) match {
				case Some(str) => str
				case None => return None
			}
			
			Some(withStem(stem))
		}
	}
	
	private def getStem(file: String): Option[String] = {
		if (name.count(_ == '%') == 1) {
			// -1 for keeping empty values
			val parts = name.split("%", -1)
			assert(parts.length == 2)
			if (file.startsWith(parts(0)) && file.endsWith(parts(1)))
				Some(file.substring(parts(0).length, file.length - parts(1).length))
			else
				None
		} else {
			None
		}
	}
	
	private def concatPrerequisites(): String = {
		if (prerequisites.length == 0)
			""
		else
			prerequisites.reduceLeft(_ + " " + _)
	}
	
	private def expandAutomaticVariables(str: String): String = {
		str
			.replaceAll("\\$@", name)
			.replaceAll("\\$<", prerequisites.headOption.getOrElse(""))
			.replaceAll("\\$\\^", concatPrerequisites)
	}
	
	private def withStem(stem: String): Rule = {
		new Rule(
			name.replaceAll("%", stem),
			prerequisites.map(_.replaceAll("%", stem)),
			recipe
		)
	}
}