package lab0

import java.time.format.DateTimeFormatter

// ============================================================
// Обработка служебных команд приложения и команды cd.
// Здесь же реализована новая команда info.
// ============================================================
object ServiceCommands:

  private val timeFmt = DateTimeFormatter.ofPattern("HH:mm")

  // Попытаться обработать команду как служебную.
  // Возвращает true, если обработана; false — если это обычная команда.
  def handle(
              cmd: String,
              args: List[String],
              session: SessionManager,
              config: AppConfig,
              engine: RuleEngine
            ): Boolean =
    cmd match

      // ----- starts -----
      case "starts" =>
        val name = if args.isEmpty then "" else args.mkString(" ")
        session.startSession(name) match
          case Left(err) => println(err)
          case Right(_) => println("Сессия начата.")
        true

      // ----- ends (номер 0) -----
      case "ends" =>
        session.endSession() match
          case Left(err) => println(err)
          case Right(finished) =>
            // Генерируем отчёт
            val md = ReportGenerator.generate(finished, config, engine)
            // Папка отчётов («report repository»)
            val outDir = os.Path(config.reportDirectory, os.pwd)
            os.makeDir.all(outDir)
            // Имя файла из названия сессии
            val safeName = finished.name.replaceAll("[^a-zA-Zа-яА-Я0-9_-]", "-").toLowerCase
            val file = outDir / s"$safeName.md"
            os.write.over(file, md)
            println("Сессия завершена.")
            println(s"Отчёт сохранён: ${file.relativeTo(os.pwd)}")
        true

      // ----- prootd -----
      case "prootd" =>
        val path = if args.isEmpty then "." else args.mkString(" ")
        session.setRoot(path) match
          case Left(err) => println(err)
          case Right(_) =>
            session.getSession.foreach { s =>
              println("Корневой каталог установлен:")
              println(s.rootDirectory)
            }
        true

      // ----- mstep -----
      case "mstep" =>
        val title = if args.isEmpty then "" else args.mkString(" ")
        session.newStep(title) match
          case Left(err) => println(err)
          case Right(_) => () // без вывода
        true

      // ----- mnote -----
      case "mnote" =>
        val text = if args.isEmpty then "" else args.mkString(" ")
        session.addNote(text) match
          case Left(err) => println(err)
          case Right(_) => println("Комментарий добавлен.")
        true

      // ----- mskip -----
      case "mskip" =>
        session.setSkipFlag()
        true

      // ----- mstatus -----
      case "mstatus" =>
        session.getSession match
          case None => println("Нет активной сессии.")
          case Some(s) =>
            println(s"Сессия: ${s.name}")
            println(s"Корневой каталог: ${s.rootDirectory}")
            println(s"Текущий каталог: ${s.currentDirectory}")
            val stepTitle = s.steps.lift(session.getCurrentStepIndex).map(_.title).getOrElse("—")
            println(s"Текущий шаг: $stepTitle")
            val included = s.steps.flatMap(_.commands).count(_.included)
            println(s"Записано команд: $included")
            println(s"Исключено команд: ${s.excludedCount}")
            println(s"Время начала: ${s.startTime.format(timeFmt)}")
        true

      // ----- mpreview -----
      case "mpreview" =>
        session.getSession match
          case None => println("Нет активной сессии.")
          case Some(s) =>
            val md = ReportGenerator.generate(s, config, engine)
            val outDir = os.Path(config.reportDirectory, os.pwd)
            os.makeDir.all(outDir)
            val safeName = s.name.replaceAll("[^a-zA-Zа-яА-Я0-9_-]", "-").toLowerCase
            val file = outDir / s"draft-$safeName.md"
            os.write.over(file, md)
            println("Предварительный отчёт создан:")
            println(s"${file.relativeTo(os.pwd)}")
        true

      // ----- info (номер 17) -----
      case "info" =>
        println("lab0 — протоколируемая терминальная сессия")
        println("Вариант 9")
        println()
        println("Краткое описание задания:")
        println("  Разработать консольное приложение, через которое")
        println("  пользователь может выполнять обычные терминальные")
        println("  команды и одновременно формировать отчёт о работе.")
        println()
        println("Автор: Дмитриев Кирилл, 24-ПО-1")
        true

      // ----- help / инструкция (номер 18 / 19) -----
      case "instructions" | "instruction" =>
        Instructions.show(args)
        true

      // ----- cd (обрабатывается отдельно) -----
      case "cd" =>
        val path = if args.isEmpty then sys.env.getOrElse("HOME", "/home") else args.head
        val cwdBefore = session.cwd

        session.changeDirectory(path) match
          case Left(err) =>
            println(err)
            Logger.warn(s"cd неудачно: $err")
          case Right(newCwd) =>
            session.recordCommand(
              text = s"cd $path", commandName = "cd", args = List(path),
              cwdBefore = cwdBefore, cwdAfter = newCwd,
              startTime = java.time.LocalDateTime.now(),
              endTime = java.time.LocalDateTime.now(),
              exitCode = 0, stdout = "", stderr = ""
            )
            session.getSession.foreach { s =>
              if !newCwd.startsWith(s.rootDirectory) then
                println("Текущий каталог находится вне корневого каталога сессии.")
                println("В отчёте будет использован абсолютный путь.")
            }
        true

      // ----- не служебная команда -----
      case _ => false