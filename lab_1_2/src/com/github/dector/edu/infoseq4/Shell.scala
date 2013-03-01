package com.github.dector.edu.infoseq4

import io.Source
import java.util.{HashMap, List, ArrayList}
import java.util
import java.io.{FileWriter, FilenameFilter, File}

/**
 * @author dector
 */
object Shell {

	// Globalization :(

	val SystemDir 	= "testSystem/"
	val EtcDir		= SystemDir + "etc/"
	val UsersFile	= EtcDir + "users"
	val RootDir		= "/"
	val ParentDir	= ".."
	val HomeDir		= "home/"

	val FolderFile	= ".folder"
	val FilenameFilter = new java.io.FilenameFilter() {
		def accept(dir: File, name: String): Boolean = name != FolderFile
	}

	val Prompt 		= "> "
	val GoodbyeMsg 	= "See ya!"
	val UserRequest	= "Enter username: "
	val PasswordRequest	= "Enter password: "
	val LoginFailedMsg	= "Login failed"
	val LoginSuccessMsg	= "Hello, %s!%n"
	val UsernameMsg	= "User: %s%n"
	val UsergroupsMsg	= "Groups: "
	val CommandNotFound	= "Command not found: "
	val AccessDenied	= "Access denied"
	val DirNotFound		= "Directory not found: "
	val Delimiter 	= " "

	val RootUser	= "root"
	val DefaultUserGroup	= "users"
	val MinUsernameLength	= 3
	val MaxUsernameLength	= 12
	val MinPasswordLength	= 3
	val MaxPasswordLength	= 12
	val UsernameExists		= "Username exists. Choose other"
	val UsernameFormatError	= "Username must starts from letter"
	val UsernameLengthError	= "Username length must be between %d and %d, but current is %d%n"
	val PasswordLengthError	= "Password length must be between %d and %d, but current is %d%n"
	val RegisterSuccessfull	= "Register successfull"
	val RegisterFailed	= "Register failed"

	val LoginCommand= "login"
	val LsCommand 	= "ls"
	val CdCommand 	= "cd"
	val UserCommand	= "user"
	val RegisterCommand	= "register"
	val ExitCommand = "exit"

	val modules = Map (
		LoginCommand -> Modules.login,
		LsCommand -> Modules.ls,
		CdCommand -> Modules.cd,
		RegisterCommand -> Modules.register,
		UserCommand -> Modules.user
	)

	val userGroups = new HashMap[String, Array[String]]()

	var currentUser = ""
	var currentDir: File = null
	var currentDirLocal = ""

	def main (args: Array[String]) {
		call(LoginCommand, Array.empty)

		var finished = currentUser == ""

		while (! finished) {
			val input = readLine(currentDirLocal + " " + Prompt)

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

	private def call(command: String, args: Array[String]) = {
		if (modules.contains(command)) {
			modules(command)(args)
		} else {
			Modules.error(command)
		}
	}

	def loadUsers() {
		Source.fromFile(UsersFile).getLines() foreach ((line) => {
			val groups = line.split(Delimiter)

			if (groups.length > 0) {
				val username = groups(0)

				val groupsArray = new Array[String](groups.length - 1)
				for (i <- 0 until groupsArray.length) {
					groupsArray(i) = groups(i + 1)
				}
				userGroups.put(username, groupsArray)
			}
		})
	}
}

object Modules {
	val cd = (args: Array[String]) => {
		if (Shell.currentDir != null && args.length > 0) {
			changeDir(args(0))
		}
	}

	val register = (args: Array[String]) => {
		if (args.length >= 2) {
			val username = args(0)
			val password = args(1)

			var valid = true

			if (findUser(username)._1 != "") {
				println(Shell.UsernameExists)
				valid = false
			}

			if (! (username.length > 0 && username(0).isLetter)) {
				println(Shell.UsernameFormatError)
				valid = false
			}

			if (! (Shell.MinUsernameLength to Shell.MaxUsernameLength contains username.length)) {
				printf(Shell.UsernameLengthError, Shell.MinPasswordLength, Shell.MaxUsernameLength, username.length)
				valid = false
			}

			if (! (Shell.MinPasswordLength to Shell.MaxPasswordLength contains password.length)) {
				printf(Shell.PasswordLengthError, Shell.MinPasswordLength, Shell.MaxPasswordLength, password.length)
				valid = false
			}

			if (valid) {
				val userStr = username + Shell.Delimiter + password + Shell.Delimiter + Shell.DefaultUserGroup
				new FileWriter(Shell.UsersFile, true).append(sys.props("line.separator") + userStr).close()
				println(Shell.RegisterSuccessfull)
			} else {
				println(Shell.RegisterFailed)
			}
		}
	}

	val ls = (args: Array[String]) => {
		if (Shell.currentDir != null && Shell.currentDir.exists()) {
			if (DirParams(Shell.currentDir).canRead(Shell.currentUser, Shell.userGroups.get(Shell.currentUser))) {
				val files = Shell.currentDir.listFiles(Shell.FilenameFilter)
				files foreach ((file) => {
					println(file.getName + (if (file.isDirectory) "/" else ""))
				})
			} else {
				println(Shell.AccessDenied)
			}
		} else {
			println(Shell.DirNotFound + Shell.currentDirLocal)
		}
	}

	val login = (args: Array[String]) => {
		var logged = false

		val user = readLine(Shell.UserRequest)
		val userPass = readLine(Shell.PasswordRequest)

		val foundUserData = findUser(user)
		if (foundUserData._1 != "" && foundUserData._1 == userPass) {
			Shell.currentUser = user

			if (foundUserData._2.length > 0) {
				Shell.userGroups.put(user, foundUserData._2)
			}

			printf(Shell.LoginSuccessMsg, Shell.currentUser)
			if (! changeDir(Shell.RootDir + Shell.HomeDir + Shell.currentUser)) {
				changeDir(Shell.RootDir)
			}
		} else {
			println(Shell.LoginFailedMsg)
		}
	}

	val user = (args: Array[String]) => {
		printf(Shell.UsernameMsg, Shell.currentUser)

		val groups = Shell.userGroups.get(Shell.currentUser)

		print(Shell.UsergroupsMsg)
		for (i <- 0 until groups.size) {
			print(groups(i) + (if (i != groups.size - 1) "," else ""))
		}
		println()
	}

	val error = (command: String) => {
		println(Shell.CommandNotFound + command)
	}

	private def changeDir(dir: String): Boolean = {
		val dirFile =
		if (dir == Shell.RootDir) {
			new File(Shell.SystemDir)
		} else if (dir.startsWith(Shell.RootDir)) {
			new File(Shell.SystemDir, dir)
		} else if (dir == Shell.ParentDir) {
			Shell.currentDir.getParentFile
		} else {
			new File(Shell.SystemDir + Shell.currentDirLocal, dir)
		}

		if (dirFile.exists()) {
			if (DirParams(dirFile).canExecute(Shell.currentUser, Shell.userGroups.get(Shell.currentUser))) {
				Shell.currentDir = dirFile

				Shell.currentDirLocal =
				if (dirFile == new File(Shell.SystemDir)) {
					Shell.RootDir
				} else {
					Shell.currentDirLocal = Shell.currentDir.getPath.replace("\\", "/")
					Shell.currentDirLocal = Shell.currentDirLocal.substring(Shell.currentDirLocal.indexOf(Shell.RootDir))
					Shell.currentDirLocal
				}
				true
			} else {
				println(Shell.AccessDenied)
				false
			}
		} else {
			println(Shell.DirNotFound + dir)
			false
		}
	}

	private def findUser(user: String): Pair[String, Array[String]] = {
		var found = false
		var password = ""
		var groups: Array[String] = null

		val linesIter = Source.fromFile(Shell.UsersFile).getLines()

		while (! found && linesIter.hasNext) {
			val line = linesIter.next()
			val parts = line.split(Shell.Delimiter)

			if (parts.length >= 3 && user == parts(0)) {
				password = parts(1)
				groups = parts.drop(2)
				found = true
			}
		}

		(password, groups)
	}
}

class DirParams(
	val user: String = "",
	val group: String = "",

	val userRead: Boolean = false,
	val userWrite: Boolean = false,
	val userExecute: Boolean = false,

	val groupRead: Boolean = false,
	val groupWrite: Boolean = false,
	val groupExecute: Boolean = false,

	val otherRead: Boolean = false,
	val otherWrite: Boolean = false,
	val otherExecute: Boolean = false
) {

	def canRead(name: String, groups: Array[String]) = {
		user == name && userRead || groups.forall(userGroup => group == userGroup && groupRead ) || otherRead
	}

	def canWrite(name: String, groups: Array[String]) = {
		user == name && userWrite || groups.forall(userGroup => group == userGroup && groupWrite ) || otherWrite
	}

	def canExecute(name: String, groups: Array[String]) = {
		user == name && userExecute || groups.forall(userGroup => group == userGroup && groupExecute ) || otherExecute
	}
}

object DirParams {
	def apply(dir: File): DirParams = {
		val folderFile = dir.listFiles(new FilenameFilter {
			def accept(dir: File, name: String): Boolean = name == Shell.FolderFile
		})

		if (folderFile.length > 0) {
			val lines = Source.fromFile(folderFile(0)).getLines()
			if (lines.hasNext) {
				val lineParts = lines.next().split(Shell.Delimiter)

				if (lineParts.length == 3) {
					val owner = lineParts(0)
					val group = lineParts(1)
					val permissions = lineParts(2)

					if (permissions.length == 9) {
						new DirParams(owner, group,
							checkRead(permissions(0)), checkWrite(permissions(1)), checkExecute(permissions(2)),
							checkRead(permissions(3)), checkWrite(permissions(4)), checkExecute(permissions(5)),
							checkRead(permissions(6)), checkWrite(permissions(7)), checkExecute(permissions(8))
						)
					} else new DirParams()
				} else new DirParams()
			} else new DirParams()
		} else new DirParams()
	}

	private def checkRead(opt: Char) = {
		opt == 'r'
	}

	private def checkWrite(opt: Char) = {
		opt == 'w'
	}

	private def checkExecute(opt: Char) = {
		opt == 'x'
	}
}