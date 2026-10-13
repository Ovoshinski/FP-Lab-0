package lab0

import java.time.format.DateTimeFormatter

//Точка входа. Проверяет незавершённую сессию, показывает справку и запускает цикл чтения команд.
object Main:

  private val dateFmt = DateTimeFormatter.ofPattern("d MMMM yyyy, HH:mm")

  def main(args: Array[String]): Unit =
    System.setProperty("file.encoding", "UTF-8")

    //Загрузка и проверка конфига
    val configPath = if args.nonEmpty then args(0) else "config.json"
    val config = ConfigLoader.load(configPath)

    val errors = ConfigLoader.validate(config)
    if errors.nonEmpty then
      println("Ошибки конфигурации:")
      errors.foreach(e => println(s"  - $e"))
      println("Используется конфигурация по умолчанию.")
      println()
    val effectiveConfig = if errors.nonEmpty then AppConfig.default else config

    //Инициализация служебных объектов
    Logger.init(effectiveConfig.logFile)
    val engine  = new RuleEngine(effectiveConfig)
    val session = new SessionManager(effectiveConfig, os.pwd.toString)

    os.makeDir.all(os.Path(effectiveConfig.reportDirectory, os.pwd))
    os.makeDir.all(os.Path(effectiveConfig.sessionDirectory, os.pwd))

    //Проверка, есть ли незавершённая сессия
    session.loadSessionFromDisk() match
      case Some(s) if s.status == "active" =>
        println("Обнаружена незавершённая сессия:")
        println()
        println(s"  Название: ${s.name}")
        println(s"  Начата:   ${s.startTime.format(dateFmt)}")
        println(s"  Шагов:    ${s.steps.size}")
        println(s"  Команд:   ${s.steps.flatMap(_.commands).size}")
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
            else
              println("Неверный выбор. Введите 1, 2 или 3.")
              println()

        choice match
          case "1" =>
            session.resumeSession(s)
            println()
            println("Сессия восстановлена.")
            println()

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
                println()
                println("Сессия завершена.")
                println(s"Отчёт сохранён: ${file.relativeTo(os.pwd)}")
                println()
              case Left(_) => ()

          case "3" =>
            println("До свидания!")
            Logger.close()
            return
          case _ => ()
      case _ => ()

    //Заголовок и список команд
    println("====================================================")
    println("Лабораторная работа - 0: протоколируемая терминальная сессия")
    println("Вариант 9")
    println("Автор: Дмитриев Кирилл, 24-ПО-1")
    println("====================================================")
    println()
    Instructions.show(Nil)

    //Главный цикл
    var running = true
    while running do
      print("report-shell> ")
      val line = scala.io.StdIn.readLine()
      if line == null then running = false
      else
        val trimmed = line.trim
        if trimmed.isEmpty then ()
        else
          val (cmd, cmdArgs) = InstructionNumbers.resolve(trimmed)
          if cmd == "exit" || cmd == "quit" then running = false
          else
            val handled = ServiceCommands.handle(cmd, cmdArgs, session, effectiveConfig, engine)
            if !handled && cmd.nonEmpty then
              CommandExecutor.execute(cmd, cmdArgs, session, effectiveConfig)

    Logger.close()
    println("До новых запусков программы!")