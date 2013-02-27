package com.github.dector.edu.infoseq4

import io.Source
import java.util.{HashMap, List, ArrayList}
import java.util

/**
 * @author dector
 */
object Shell {

	val SystemDir 	= "testSystem/"
	val EtcDir		= SystemDir + "etc/"
	val UsersFile	= EtcDir + "users"
	val PasswordsFile	= EtcDir + "passwd"

	val Prompt 		= "> "
	val GoodbyeMsg 	= "See ya!"
	val UserRequest	= "Enter username: "
	val PasswordRequest	= "Enter password: "
	val LoginFailedMsg	= "Login failed"
	val LoginSuccessMsg	= "Hello, %s!%n"
	val Delimiter 	= " "

	val LoginCommand= "login"
	val LsCommand 	= "ls"
	val ExitCommand = "exit"

	val modules = Map (
		LoginCommand -> Modules.login,
		LsCommand -> Modules.ls
	)

	val userGroups = new HashMap[String, List[String]]()

	var currentUser = ""

	def main (args: Array[String]) {
		call(LoginCommand, Array.empty)

		var finished = currentUser == ""
		if (finished) {
			println(LoginFailedMsg)
		} else {
			printf(LoginSuccessMsg, currentUser)
			loadUsers()
		}

		while (! finished) {
			val input = readLine(Prompt)

			if (input == ExitCommand) {
				finished = true 
			} else {
				val command = input split Delimiter

				if (command.length > 0) {
					call(command(0), command drop 1)
				}
			}
		}

		println(GoodbyeMsg)
	}

	private def call(command: String, args: Array[String]) = modules(command)(args)

	private def loadUsers() {
		Source.fromFile(UsersFile).getLines() foreach ((line) => {
			val groups = line.split(Delimiter)

			if (groups.length > 0) {
				val username = groups(0)

				userGroups.put(username, new ArrayList[String]())
				groups.drop(1) foreach ((group) => {
					userGroups.get(username).add(group)
				})
			}
		})
	}
}

object Modules {
	val ls = (args: Array[String]) => {
		println("ls executed with: " + args.mkString(", "))
	}

	val login = (args: Array[String]) => {
		var logged = false

		val user = readLine(Shell.UserRequest)
		val userPass = readLine(Shell.PasswordRequest)

		val linesIter = Source.fromFile(Shell.PasswordsFile).getLines().toIterator;

		while (! logged && linesIter.hasNext) {
			val line = linesIter.next()

			val parts = line.split(Shell.Delimiter)

			if (parts.length == 2 && user == parts(0) && userPass == parts(1)) {
				// Globalization :)
				Shell.currentUser = parts(0)
				logged = true
			}
		}

		logged
	}
}