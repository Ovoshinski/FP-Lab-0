package lab0

// Точка входа и главный цикл REPL.
object Main:

  def main(args: Array[String]): Unit =

    // Принудительно UTF-8 для ввода/вывода
    System.setProperty("file.encoding", "UTF-8")

    // 1. Загружаем конфиг
    val configPath = if args.nonEmpty then args(0) else "config.json"
    val config = ConfigLoader.load(configPath)

    // 2. Валидация
    val errors = ConfigLoader.validate(config)
    if errors.nonEmpty then
      println("Ошибки конфигурации:")
      errors.foreach(e => println(s"  - $e"))
      println("Используется конфигурация по умолчанию.")
    val effectiveConfig = if errors.nonEmpty then AppConfig.default else config

    // 3. Логгер (создаст logs/ автоматически)
    Logger.init(effectiveConfig.logFile)

    // 4. Движок правил
    val engine = new RuleEngine(effectiveConfig)

    // 5. Менеджер сессий
    val initialCwd = os.pwd.toString
    val session = new SessionManager(effectiveConfig, initialCwd)

    // 6. Рабочие папки
    os.makeDir.all(os.Path(effectiveConfig.reportDirectory, os.pwd))
    os.makeDir.all(os.Path(effectiveConfig.sessionDirectory, os.pwd))

    // 7. Приветствие (без "report-shell v1.0.0")
    println("Введите '18' для списка команд, '17' для информации, '19' для выхода.")
    println()

    // 8. Главный цикл
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

    // 9. Закрытие
    Logger.close()
    println("До свидания!")