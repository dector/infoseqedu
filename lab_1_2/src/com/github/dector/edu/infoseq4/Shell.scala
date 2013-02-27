package com.github.dector.edu.infoseq4

/**
 * @author dector
 */
object Shell {

	val Prompt = "> "
	val Delimiter = " "

	val LsCommand = "ls"
	val ExitCommand = "exit"

	val modules = Map (
		LsCommand -> Modules.ls
	)

	def main (args: Array[String]) {
		var finished = false

		while (! finished) {
			val input = readLine(Prompt)

			if (input == ExitCommand) {
				finished = true 
			} else {
				val command = input split Delimiter

				if (command.length > 0) {
					modules(command(0))(command drop 1)
				}
			}
		}
	}
}

object Modules {
	val ls = (args: Array[String]) => {
		println("ls executed with: " + args.mkString(", "))
	}
}