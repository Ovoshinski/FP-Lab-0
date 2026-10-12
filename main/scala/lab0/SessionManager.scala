package lab0

import java.time.LocalDateTime

// ============================================================
// Менеджер сессии: начало, конец, шаги, команды, cd.
// Хранит текущую сессию в памяти и предоставляет методы работы с ней.
// ============================================================
class SessionManager(config: AppConfig, initialCwd: String):

  // Текущая сессия (None = не активна)
  private var currentSession: Option[Session] = None

  // Индекс текущего шага (-1 = шага ещё нет)
  private var currentStepIndex: Int = -1

  // Флаг mskip: следующая команда пропускается
  private var skipNext: Boolean = false

  // Счётчик команд в сессии
  private var commandCounter: Int = 0

  // Текущий рабочий каталог
  def cwd: String =
    currentSession.map(_.currentDirectory).getOrElse(initialCwd)

  // Активна ли сессия
  def isActive: Boolean =
    currentSession.exists(_.status == "active")

  // Установить флаг mskip
  def setSkipFlag(): Unit = skipNext = true

  // -------------------- Управление сессией --------------------

  // Начать новую сессию
  def startSession(name: String): Either[String, Unit] =
    if isActive then
      Left("Сессия уже активна. Завершите её командой ends.")
    else
      val session = Session(
        name = if name.trim.isEmpty then "Без названия" else name.trim,
        startTime = LocalDateTime.now(),
        endTime = None,
        rootDirectory = cwd,
        currentDirectory = cwd,
        shell = config.shell,
        profile = config.defaultProfile
      )
      currentSession = Some(session)
      currentStepIndex = -1
      commandCounter = 0
      Logger.info(s"Начата сессия: \"${session.name}\"")
      Right(())

  // Установить корневой каталог
  def setRoot(path: String): Either[String, Unit] =
    currentSession match
      case None => Left("Нет активной сессии.")
      case Some(s) =>
        val resolved = resolvePath(path)
        val p = os.Path(resolved)
        if !os.exists(p) then Left(s"Каталог не существует: $path")
        else if !os.isDir(p) then Left(s"Не является каталогом: $path")
        else
          currentSession = Some(s.copy(rootDirectory = resolved))
          Logger.info(s"Root установлен: $resolved")
          Right(())

  // -------------------- Шаги и заметки --------------------

  // Создать новый шаг
  def newStep(title: String): Either[String, Unit] =
    currentSession match
      case None => Left("Нет активной сессии.")
      case Some(s) =>
        val idx = s.steps.size
        val actualTitle = if title.trim.isEmpty then s"Шаг ${idx + 1}" else title.trim
        val step = Step(index = idx, title = actualTitle)
        currentSession = Some(s.copy(steps = s.steps :+ step))
        currentStepIndex = idx
        Logger.info(s"Создан шаг #$idx: \"$actualTitle\"")
        Right(())

  // Добавить комментарий в текущий шаг
  def addNote(text: String): Either[String, Unit] =
    currentSession match
      case None => Left("Нет активной сессии.")
      case Some(s) =>
        // Если шага нет — создаём "Шаг 1"
        if currentStepIndex < 0 then newStep("Шаг 1")
        currentSession match
          case Some(s2) =>
            val step = s2.steps(currentStepIndex)
            val newStep = step.copy(notes = step.notes :+ text)
            val newSteps = s2.steps.updated(currentStepIndex, newStep)
            currentSession = Some(s2.copy(steps = newSteps))
            Right(())
          case None => Left("Ошибка")

  // -------------------- Команды --------------------

  // Записать выполненную команду в текущий шаг
  def recordCommand(
                     text: String, commandName: String, args: List[String],
                     cwdBefore: String, cwdAfter: String,
                     startTime: LocalDateTime, endTime: LocalDateTime,
                     exitCode: Int, stdout: String, stderr: String
                   ): Unit =
    currentSession.foreach { s =>
      if skipNext then
        // Команда исключена из отчёта
        Logger.info(s"Команда id=$commandCounter исключена из отчёта")
        commandCounter += 1
        skipNext = false
      else
        val rec = CommandRecord(
          id = commandCounter, text = text, commandName = commandName,
          arguments = args, cwdBefore = cwdBefore, cwdAfter = cwdAfter,
          startTime = startTime, endTime = endTime,
          exitCode = exitCode, stdout = stdout, stderr = stderr,
          stepIndex = if currentStepIndex < 0 then 0 else currentStepIndex,
          included = true
        )
        // Если шагов ещё нет — создаём автоматически "Шаг 1"
        val steps =
          if s.steps.isEmpty then
            List(Step(0, "Шаг 1", Nil, List(rec)))
          else
            val idx = currentStepIndex.max(0)
            val step = s.steps(idx)
            val newStep = step.copy(commands = step.commands :+ rec)
            s.steps.updated(idx, newStep)
        currentSession = Some(s.copy(steps = steps))
        commandCounter += 1
    }

  // Изменить текущий каталог (для cd)
  def changeDirectory(path: String): Either[String, String] =
    val resolved = resolvePath(path)
    val p = os.Path(resolved)
    if !os.exists(p) then Left(s"Каталог не существует: $path")
    else if !os.isDir(p) then Left(s"Не является каталогом: $path")
    else
      currentSession = currentSession.map(_.copy(currentDirectory = resolved))
      Right(resolved)

  // Завершить сессию
  def endSession(): Either[String, Session] =
    currentSession match
      case None => Left("Нет активной сессии.")
      case Some(s) =>
        val finished = s.copy(endTime = Some(LocalDateTime.now()), status = "finished")
        currentSession = Some(finished)
        Logger.info(s"Сессия завершена: \"${s.name}\"")
        Right(finished)

  // Геттеры
  def getSession: Option[Session] = currentSession
  def getCurrentStepIndex: Int = currentStepIndex

  // -------------------- Утилиты --------------------

  // Разрешить путь относительно cwd, обработать "~"
  private def resolvePath(path: String): String =
    val trimmed = path.trim
    val base = if trimmed.startsWith("~") then sys.env.getOrElse("HOME", "/home") else cwd
    val expanded = if trimmed.startsWith("~") then trimmed.replaceFirst("^~", base) else trimmed
    os.Path(expanded, os.Path(base)).toString