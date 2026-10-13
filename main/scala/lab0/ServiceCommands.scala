package lab0

import java.time.format.DateTimeFormatter

//Обработка служебных команд и cd
object ServiceCommands:

  private val timeFmt = DateTimeFormatter.ofPattern("HH:mm")


  def handle(
              cmd: String, args: List[String],
              session: SessionManager, config: AppConfig, engine: RuleEngine
            ): Boolean =
    cmd match

      //1. cd — смена каталога
      case "cd" =>
        val rawArg = if args.isEmpty then "home" else args.head

        if rawArg == "home" then
          //Переход в корень сессии, заданный через prootd
          session.getSession match
            case None =>
              println("Корневой каталог не указан.")
              println("Сначала начните сессию (команда 10) и установите корневой каталог (команда 11).")
              println()
            case Some(s) =>
              val cwdBefore = session.cwd
              session.changeDirectory(s.rootDirectory) match
                case Left(err) =>
                  println(err); println()
                  Logger.warn(s"cd home неудачно: $err")
                case Right(newCwd) =>
                  session.recordCommand(
                    text = "cd home", commandName = "cd", args = List("home"),
                    cwdBefore = cwdBefore, cwdAfter = newCwd,
                    startTime = java.time.LocalDateTime.now(),
                    endTime   = java.time.LocalDateTime.now(),
                    exitCode = 0, stdout = "", stderr = ""
                  )
                  println(s"Переход в корневой каталог сессии: $newCwd")
                  println()
          true

        else
          val path = if rawArg == "up" then ".." else rawArg
          val cwdBefore = session.cwd

          session.changeDirectory(path) match
            case Left(err) =>
              println(err); println()
              Logger.warn(s"cd неудачно: $err")
            case Right(newCwd) =>
              session.recordCommand(
                text = s"cd $rawArg", commandName = "cd", args = List(rawArg),
                cwdBefore = cwdBefore, cwdAfter = newCwd,
                startTime = java.time.LocalDateTime.now(),
                endTime   = java.time.LocalDateTime.now(),
                exitCode = 0, stdout = "", stderr = ""
              )
              session.getSession.foreach { s =>
                if !newCwd.startsWith(s.rootDirectory) then
                  println("Текущий каталог находится вне корневого каталога сессии.")
                  println("В отчёте будет использован абсолютный путь.")
                  println()
              }
          true


      //10. starts — начать сессию
      case "starts" =>
        val name = if args.isEmpty then "" else args.mkString(" ")
        session.startSession(name) match
          case Left(err) => println(err); println()
          case Right(_)  => println("Сессия с записью протокола начата."); println()
        true


      //11. prootd — установить корневой каталог
      case "prootd" =>
        val path = if args.isEmpty then "." else args.mkString(" ")
        session.setRoot(path) match
          case Left(err) => println(err); println()
          case Right(_) =>
            session.getSession.foreach { s =>
              println("Корневой каталог установлен:")
              println(s.rootDirectory)
              println()
            }
        true


      //12. mstep — новый шаг
      case "mstep" =>
        val title = if args.isEmpty then "" else args.mkString(" ")
        session.newStep(title) match
          case Left(err) => println(err); println()
          case Right(_)  => println("Новый шаг создан."); println()
        true


      //13. mnote — комментарий
      case "mnote" =>
        val text = if args.isEmpty then "" else args.mkString(" ")
        session.addNote(text) match
          case Left(err) => println(err); println()
          case Right(_)  => println("Комментарий добавлен."); println()
        true


      //14. mskip — пометить следующую команду как "не в отчёт"
      case "mskip" =>
        session.setSkipFlag()
        println("Следующая команда будет исключена из отчёта.")
        println()
        true


      //15. mstatus — состояние сессии
      case "mstatus" =>
        session.getSession match
          case None =>
            println("Нет активной сессии.")
            println()
          case Some(s) =>
            println(s"Сессия: ${s.name}")
            println(s"Корневой каталог: ${s.rootDirectory}")
            println(s"Текущий каталог: ${s.currentDirectory}")
            val stepTitle = s.steps.lift(session.getCurrentStepIndex).map(_.title).getOrElse("-")
            println(s"Текущий шаг: $stepTitle")
            val included = s.steps.flatMap(_.commands).count(_.included)
            println(s"Записано команд: $included")
            println(s"Исключено команд: ${s.excludedCount}")
            println(s"Время начала: ${s.startTime.format(timeFmt)}")
            println()
        true


      //16. mpreview — предварительный отчёт
      case "mpreview" =>
        session.getSession match
          case None =>
            println("Нет активной сессии.")
            println()
          case Some(s) =>
            val md = ReportGenerator.generate(s, config, engine)
            val outDir = os.Path(config.reportDirectory, os.pwd)
            os.makeDir.all(outDir)
            val safeName = s.name.replaceAll("[^a-zA-Zа-яА-Я0-9_-]", "-").toLowerCase
            val file = outDir / s"draft-$safeName.md"
            os.write.over(file, md)
            println("Предварительный отчёт создан:")
            println(s"${file.relativeTo(os.pwd)}")
            println()
        true


      //17. info — информация о работе
      case "info" =>
        println("Лабораторная работа: протоколируемая терминальная сессия")
        println("Вариант 9")
        println()
        println("Краткое описание задания:")
        println("  Разработать консольное приложение, через которое пользователь может выполнять обычные терминальные команды и одновременно формировать отчёт о проделанной работе.")
        println()
        println("Автор: Дмитриев Кирилл, 24-ПО-1")
        println()
        true


      //18. instructions — справка
      case "instructions" | "instruction" =>
        Instructions.show(args)
        true


      //19. ends — завершить сессию и сохранить отчёт
      case "ends" =>
        session.endSession() match
          case Left(err) => println(err); println()
          case Right(finished) =>
            val md = ReportGenerator.generate(finished, config, engine)
            val outDir = os.Path(config.reportDirectory, os.pwd)
            os.makeDir.all(outDir)
            val safeName = finished.name.replaceAll("[^a-zA-Zа-яА-Я0-9_-]", "-").toLowerCase
            val file = outDir / s"$safeName.md"
            os.write.over(file, md)
            println("Сессия завершена.")
            println(s"Отчёт сохранён: ${file.relativeTo(os.pwd)}")
            println()
        true


      case _ => false