package lab0

import java.time.LocalDateTime
import scala.util.Try

// Запуск внешних команд. Сначала переводим их через CommandTranslator,
// потом запускает процесс и захватываем stdout, stderr, код возврата
object CommandExecutor:

  def execute(
               commandName: String,
               args: List[String],
               session: SessionManager,
               config: AppConfig
             ): Unit =

    val cwdBefore = session.cwd
    val startTime = LocalDateTime.now()
    
    val (exec, finalArgs) = CommandTranslator.translate(commandName, args, cwdBefore)
    
    val result: Either[String, (Int, String, String)] =
      Try {
        val proc = os.proc(exec, finalArgs).call(
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
      //Запустить не удалось
      case Left(errMsg) =>
        println(s"Не удалось запустить команду: $errMsg")
        println()
        Logger.error(s"Ошибка запуска '$commandName': $errMsg")
        session.recordCommand(
          text = (commandName :: args).mkString(" "),
          commandName = commandName, args = args,
          cwdBefore = cwdBefore, cwdAfter = cwdBefore,
          startTime = startTime, endTime = endTime,
          exitCode = 127, stdout = "", stderr = errMsg
        )

      //Команда отработала
      case Right((exitCode, stdout, stderr)) =>
        if stdout.nonEmpty then print(stdout)
        if stderr.nonEmpty then System.err.print(stderr)
        if exitCode != 0 then println(s"\nКоманда завершилась с кодом $exitCode.")
        println()

        session.recordCommand(
          text = (commandName :: args).mkString(" "),
          commandName = commandName, args = args,
          cwdBefore = cwdBefore, cwdAfter = session.cwd,
          startTime = startTime, endTime = endTime,
          exitCode = exitCode, stdout = stdout, stderr = stderr
        )