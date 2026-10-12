package lab0

import java.time.LocalDateTime
import scala.util.Try

// ============================================================
// Запуск внешних команд через os-lib (прямой запуск процесса).
// Захватывает stdout, stderr, код возврата.
// Результат записывается в сессию.
// ============================================================
object CommandExecutor:

  // Выполнить внешнюю команду и записать её в сессию
  def execute(
               commandName: String,
               args: List[String],
               session: SessionManager,
               config: AppConfig
             ): Unit =

    // Запомнить каталог до запуска и время старта
    val cwdBefore = session.cwd
    val startTime = LocalDateTime.now()

    // Перевод в аналоги Windows (на Linux/macOS не меняет ничего)
    val (exec, finalArgs) = CommandTranslator.translate(commandName, args)
    
    // Попытаться запустить процесс
    val result: Either[String, (Int, String, String)] =
      Try {
        val proc = os.proc(commandName, args).call(
          cwd = os.Path(cwdBefore),   // рабочий каталог
          check = false,              // не бросать исключение при ненулевом коде
          stdout = os.Pipe,           // захватить stdout
          stderr = os.Pipe,           // захватить stderr
          mergeErrIntoOut = false     // держать их раздельно
        )
        (proc.exitCode, proc.out.text(), proc.err.text())
      }.toEither.left.map(_.getMessage)

    val endTime = LocalDateTime.now()

    result match
      // Не удалось запустить процесс (команды нет, нет прав и т.п.)
      case Left(errMsg) =>
        println(s"Не удалось запустить команду: $errMsg")
        Logger.error(s"Ошибка запуска '$commandName': $errMsg")
        session.recordCommand(
          text = (commandName :: args).mkString(" "),
          commandName = commandName, args = args,
          cwdBefore = cwdBefore, cwdAfter = cwdBefore,
          startTime = startTime, endTime = endTime,
          exitCode = 127, stdout = "", stderr = errMsg
        )

      // Команда отработала — показать и записать результат
      case Right((exitCode, stdout, stderr)) =>
        // Показать пользователю
        if stdout.nonEmpty then print(stdout)
        if stderr.nonEmpty then System.err.print(stderr)
        if exitCode != 0 then
          println(s"\nКоманда завершилась с кодом $exitCode.")

        // Записать в сессию
        session.recordCommand(
          text = (commandName :: args).mkString(" "),
          commandName = commandName, args = args,
          cwdBefore = cwdBefore, cwdAfter = session.cwd,
          startTime = startTime, endTime = endTime,
          exitCode = exitCode, stdout = stdout, stderr = stderr
        )