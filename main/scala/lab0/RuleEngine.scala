package lab0

// ============================================================
// Движок правил: выбирает правило под команду и подставляет
// значения в шаблон описания.
// ============================================================
class RuleEngine(config: AppConfig):

  // Все правила из конфига
  private val allRules: List[RuleConfig] = config.rules

  // -------------------- Выбор правила --------------------

  // Выбрать наиболее подходящее правило для команды
  def selectRule(rec: CommandRecord): Option[RuleConfig] =
    // 1. Отбираем подходящие
    val candidates = allRules.filter(r => matches(r, rec))

    if candidates.isEmpty then None
    else
      // 2. Сортируем по убыванию приоритета, затем по конкретности
      val sorted = candidates.sortBy(r => (-r.priority, -specificity(r)))
      val winner = sorted.head

      // 3. Если есть конфликт — пишем в журнал
      val conflicts = sorted.filter(_.priority == winner.priority)
      if conflicts.size > 1 then
        Logger.warn(s"Конфликт правил для '${rec.commandName}': ${conflicts.map(_.id).mkString(", ")}")

      Logger.info(s"Применено правило ${winner.id} для команды id=${rec.id}")
      Some(winner)

  // Проверка: подходит ли правило к команде
  private def matches(r: RuleConfig, rec: CommandRecord): Boolean =
    if !r.enabled then false
    else if r.command != "*" && r.command != rec.commandName then false
    else
      r.arguments match
        case None => true
        case Some(cond) =>
          val args = rec.arguments
          val allOk = cond.containsAll.isEmpty || cond.containsAll.forall(args.contains)
          val anyOk = cond.containsAny.isEmpty || cond.containsAny.exists(args.contains)
          val noneOk = cond.containsNone.isEmpty || cond.containsNone.forall(a => !args.contains(a))
          allOk && anyOk && noneOk

  // «Конкретность» правила — сколько условий задано
  private def specificity(r: RuleConfig): Int =
    r.arguments match
      case None => 0
      case Some(c) => c.containsAll.size + c.containsAny.size + c.containsNone.size

  // -------------------- Подстановки в шаблон --------------------

  // Подставить значения в шаблон описания
  def fillTemplate(template: String, rec: CommandRecord, session: Session): String =
    val relativeDir = makeRelative(session.currentDirectory, session.rootDirectory)

    val values = Map(
      "command" -> rec.text,
      "commandName" -> rec.commandName,
      "arguments" -> rec.arguments.mkString(" "),
      "argument1" -> rec.arguments.headOption.getOrElse(""),
      "argument2" -> rec.arguments.drop(1).headOption.getOrElse(""),
      "currentDirectory" -> rec.cwdBefore,
      "relativeDirectory" -> relativeDir,
      "exitCode" -> rec.exitCode.toString,
      "duration" -> java.time.Duration.between(rec.startTime, rec.endTime).toMillis.toString,
      "stepTitle" -> session.steps.lift(rec.stepIndex).map(_.title).getOrElse("")
    )

    // Заменяем {ключ} → значение
    values.foldLeft(template) { case (acc, (key, value)) =>
      acc.replace(s"{$key}", value)
    }

  // -------------------- Утилиты --------------------

  // Сделать путь относительным к корню сессии
  private def makeRelative(path: String, root: String): String =
    if path.startsWith(root) then
      val rel = path.substring(root.length).stripPrefix("/")
      if rel.isEmpty then "." else rel
    else path