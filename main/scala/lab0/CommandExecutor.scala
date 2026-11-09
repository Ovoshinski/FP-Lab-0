package lab0

import java.time.LocalDateTime
import scala.util.Try

//Запуск внешних команд напрямую через cmd.exe
object CommandExecutor:

  def execute(rawCommand: String, session: SessionManager, config: AppConfig): Unit =

    val cwdBefore = session.cwd
    val startTime = LocalDateTime.now()

    //Запускаем cmd.exe с флагом /c (выполнить команду и закрыться)
    val result: Either[String, (Int, String, String)] =
      Try {
        val proc = os.proc(config.shell, "/c", rawCommand).call(
          cwd = os.Path(cwdBefore),
          check = false,
          stdout = os.Pipe,
          stderr = os.Pipe,
          mergeErrIntoOut = false
        )
        (proc.exitCode, proc.out.text(), proc.err.text())
      }.toEither.left.map(_.getMessage)

    val endTime = LocalDateTime.now()

    result match
      case Left(errMsg) =>
        println(s"Не удалось запустить команду: $errMsg")
        println()
        Logger.error(s"Ошибка запуска '$rawCommand': $errMsg")
        session.recordCommand(
          text = rawCommand,
          commandName = rawCommand.split(" ").headOption.getOrElse(""),
          args = rawCommand.split(" ").drop(1).toList,
          cwdBefore = cwdBefore, cwdAfter = cwdBefore,
          startTime = startTime, endTime = endTime,
          exitCode = 127, stdout = "", stderr = errMsg
        )

      case Right((exitCode, stdout, stderr)) =>
        if stdout.nonEmpty then print(stdout)
        if stderr.nonEmpty then System.err.print(stderr)
        if exitCode != 0 then println(s"\nКоманда завершилась с кодом $exitCode.")
        println()

        session.recordCommand(
          text = rawCommand,
          commandName = rawCommand.split(" ").headOption.getOrElse(""),
          args = rawCommand.split(" ").drop(1).toList,
          cwdBefore = cwdBefore, cwdAfter = session.cwd,
          startTime = startTime, endTime = endTime,
          exitCode = exitCode, stdout = stdout, stderr = stderr
        )