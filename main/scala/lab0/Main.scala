package lab0

import java.time.format.DateTimeFormatter

object Main:

  private val dateFmt = DateTimeFormatter.ofPattern("d MMMM yyyy, HH:mm")

  def main(args: Array[String]): Unit =
    System.setProperty("file.encoding", "UTF-8")

    val configPath = if args.nonEmpty then args(0) else "config.json"
    val config = ConfigLoader.load(configPath)

    val errors = ConfigLoader.validate(config)
    if errors.nonEmpty then
      println("Ошибки конфигурации:")
      errors.foreach(e => println(s"  - $e"))
      println("Используется конфигурация по умолчанию.")
      println()
    val effectiveConfig = if errors.nonEmpty then AppConfig.default else config

    Logger.init(effectiveConfig.logFile)
    val engine  = new RuleEngine(effectiveConfig)
    val session = new SessionManager(effectiveConfig, os.pwd.toString)

    os.makeDir.all(os.Path(effectiveConfig.reportDirectory, os.pwd))
    os.makeDir.all(os.Path(effectiveConfig.sessionDirectory, os.pwd))

    //Проверка на наличие незавершенной сессии
    session.loadSessionFromDisk() match
      case Some(s) if s.status == "active" =>
        println("Обнаружена незавершённая сессия:")
        println(s"  Название: ${s.name}")
        println(s"  Начата:   ${s.startTime.format(dateFmt)}")
        println()
        println("Что сделать с этой сессией?")
        println("  [1] Продолжить (вернуться в неё)")
        println("  [2] Завершить и сохранить отчёт")
        println("  [3] Выключить программу (данные сохранятся)")
        println()

        var choice = ""
        while choice != "1" && choice != "2" && choice != "3" do
          print("Ваш выбор (1, 2 или 3): ")
          val input = scala.io.StdIn.readLine()
          if input == null then choice = "3"
          else
            val trimmed = input.trim
            if trimmed == "1" || trimmed == "2" || trimmed == "3" then choice = trimmed
            else println("Неверный выбор. Введите 1, 2 или 3.")

        choice match
          case "1" =>
            session.resumeSession(s)
            println("\nСессия восстановлена.\n")
          case "2" =>
            session.resumeSession(s)
            session.endSession() match
              case Right(finished) =>
                val md = ReportGenerator.generate(finished, effectiveConfig, engine)
                val outDir = os.Path(effectiveConfig.reportDirectory, os.pwd)
                os.makeDir.all(outDir)
                val safeName = finished.name.replaceAll("[^a-zA-Zа-яА-Я0-9_-]", "-").toLowerCase
                val file = outDir / s"$safeName.md"
                os.write.over(file, md)
                println("\nСессия завершена.")
                println(s"Отчёт сохранён: ${file.relativeTo(os.pwd)}\n")
              case Left(_) => ()
          case "3" =>
            println("До свидания!")
            Logger.close()
            return
          case _ => ()
      case _ => ()

    //Заголовок
    println("====================================================")
    println("Лабораторная работа - 0: протоколируемая терминальная сессия")
    println("Вариант 9")
    println("====================================================")
    println()
    Instructions.show(Nil)

    //Главный цикл
    var running = true
    while running do
      print("report-shell> ")
      val line = scala.io.StdIn.readLine()
      if line == null then
        running = false
      else
        val trimmed = line.trim
        if trimmed.isEmpty then ()
        else
          val (cmd, cmdArgs) = InstructionNumbers.resolve(trimmed)
          if cmd == "exit" || cmd == "quit" then
            running = false
          else
            val handled = ServiceCommands.handle(cmd, cmdArgs, session, effectiveConfig, engine)
            if !handled && cmd.nonEmpty then
              //Превращаем номер в команду
              val finalCommand = if trimmed.matches("\\d+.*") then (cmd :: cmdArgs).mkString(" ") else trimmed
              CommandExecutor.execute(finalCommand, session, effectiveConfig)

    Logger.close()
    println("До новых запусков программы!")