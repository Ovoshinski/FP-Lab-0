package lab0

import java.time.format.DateTimeFormatter

//Обработка служебных команд
object ServiceCommands:

  private val timeFmt = DateTimeFormatter.ofPattern("HH:mm")

  def handle(
              cmd: String, args: List[String],
              session: SessionManager, config: AppConfig, engine: RuleEngine
            ): Boolean =
    cmd match

      //1. cd — смена каталога
      case "cd" =>
        if args.isEmpty then
          println(session.cwd)
          println()
          true
        else
          val rawPath = args.head
          val targetPath = if rawPath == "~" then sys.env.getOrElse("USERPROFILE", rawPath) else rawPath
          val cwdBefore = session.cwd

          session.changeDirectory(targetPath) match
            case Left(err) =>
              println(err);
              println()
              Logger.warn(s"cd неудачно: $err")
            case Right(newCwd) =>
              session.recordCommand(
                text = s"cd $rawPath", commandName = "cd", args = List(rawPath),
                cwdBefore = cwdBefore, cwdAfter = newCwd,
                startTime = java.time.LocalDateTime.now(),
                endTime = java.time.LocalDateTime.now(),
                exitCode = 0, stdout = "", stderr = ""
              )
              session.getSession.foreach { s =>
                if !newCwd.startsWith(s.rootDirectory) then
                  println("Текущий каталог находится вне корневого каталога сессии.")
                  println("В отчёте будет использован абсолютный путь.")
                  println()
              }
          true

      //2. pwd — вывод пути
      case "pwd" =>
        println(session.cwd)
        println()
        true

      //9. starts
      case "starts" =>
        val name = if args.isEmpty then "" else args.mkString(" ")
        session.startSession(name) match
          case Left(err) => println(err); println()
          case Right(_) => println("Сессия с записью протокола начата."); println()
        true

      //10. prootd
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

      //11. mstep
      case "mstep" =>
        val title = if args.isEmpty then "" else args.mkString(" ")
        session.newStep(title) match
          case Left(err) => println(err); println()
          case Right(_) => println("Новый шаг создан."); println()
        true

      //12. mnote
      case "mnote" =>
        val text = if args.isEmpty then "" else args.mkString(" ")
        session.addNote(text) match
          case Left(err) => println(err); println()
          case Right(_) => println("Комментарий добавлен."); println()
        true

      //13. mskip
      case "mskip" =>
        session.setSkipFlag()
        println("Следующая команда будет исключена из отчёта.")
        println()
        true

      //14. mstatus
      case "mstatus" =>
        session.getSession match
          case None =>
            println("Нет активной сессии."); println()
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

      //15. mpreview
      case "mpreview" =>
        session.getSession match
          case None => println("Нет активной сессии."); println()
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

      //16. info
      case "info" =>
        println("Лабораторная работа: протоколируемая терминальная сессия")
        println("Вариант 9")
        println()
        println("Краткое описание задания:")
        println("Разработать консольное приложение, через которое пользователь может выполнять обычные терминальные команды и одновременно формировать отчёт о проделанной работе.")
        println()
        println("Автор: Дмитриев Кирилл, 24-ПО-1")
        println()
        true

      //17. instructions
      case "instructions" | "instruction" =>
        Instructions.show(args)
        true

      //18. ends
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
            println("Отчет появится после выхода из программы (команда exit [ 0])\n")
            println()
        true

      case _ => false