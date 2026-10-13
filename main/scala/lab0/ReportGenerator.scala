package lab0

import java.time.format.DateTimeFormatter

//Генерация Markdown-отчёта из готовой сессии
object ReportGenerator:

  private val dateFmt = DateTimeFormatter.ofPattern("d MMMM yyyy, HH:mm")


  def generate(session: Session, config: AppConfig, engine: RuleEngine): String =
    val sb = new StringBuilder

    //Заголовок
    val profile = config.profiles.getOrElse(session.profile, config.profiles(config.defaultProfile))
    val title = profile.titleTemplate.replace("{sessionName}", session.name)
    sb.append(s"# $title\n\n")

    //Сведения о сессии
    sb.append("## Сведения о сессии\n\n")
    sb.append(s"- Начало: ${session.startTime.format(dateFmt)}\n")
    session.endTime.foreach(t => sb.append(s"- Завершение: ${t.format(dateFmt)}\n"))
    sb.append(s"- Корневой каталог: `${session.rootDirectory}`\n")
    sb.append(s"- Командный интерпретатор: `${session.shell}`\n")
    sb.append(s"- Профиль: `${session.profile}`\n\n")

    //Шаги — пустые пропускаем
    val nonEmptySteps = session.steps.filterNot(_.isEmpty)
    nonEmptySteps.zipWithIndex.foreach { case (step, idx) =>
      sb.append(s"## Шаг ${idx + 1}. ${step.title}\n\n")

      step.notes.foreach(note => sb.append(s"$note\n\n"))

      step.commands.filter(_.included).foreach { rec =>
        engine.selectRule(rec) match
          case Some(rule) =>
            val desc = engine.fillTemplate(rule.descriptionTemplate, rec, session)
            sb.append(s"$desc\n\n")
            if rule.showCommand then sb.append(s"```bash\n${rec.text}\n```\n\n")
            appendOutput(sb, rec, rule.outputMode, rule.outputLanguage, rule.maximumLines)

          case None =>
            sb.append(s"Выполним команду:\n\n")
            sb.append(s"```bash\n${rec.text}\n```\n\n")
            appendOutput(sb, rec, "full", "text", None)
      }
    }
    
    val errors = session.steps.flatMap(_.commands).filter(c => c.included && c.exitCode != 0)
    if errors.nonEmpty then
      sb.append("## Ошибки и предупреждения\n\n")
      errors.foreach { rec =>
        sb.append(s"Команда `${rec.text}` завершилась с кодом `${rec.exitCode}`.\n\n")
        if rec.stderr.nonEmpty then
          sb.append(s"```text\n${escapeFences(rec.stderr)}\n```\n\n")
      }

    //Итоги
    val allCommands = session.steps.flatMap(_.commands)
    val included = allCommands.count(_.included)
    val success  = allCommands.count(c => c.included && c.exitCode == 0)
    val failed   = allCommands.count(c => c.included && c.exitCode != 0)

    sb.append("## Итоги\n\n")
    sb.append(s"- Выполнено команд: $included\n")
    sb.append(s"- Успешно: $success\n")
    sb.append(s"- Завершилось с ошибкой: $failed\n")
    sb.append(s"- Исключено из отчёта: ${session.excludedCount}\n")

    sb.toString


  //Добавить блок с выводом команды в зависимости от режима
  private def appendOutput(
                            sb: StringBuilder, rec: CommandRecord,
                            mode: String, lang: String, maxLines: Option[Int]
                          ): Unit =
    val text = mode match
      case "none"        => ""
      case "errors-only" => rec.stderr
      case "full"        => rec.stdout + (if rec.stderr.nonEmpty then "\n" + rec.stderr else "")
      case "first-lines" => rec.stdout.linesIterator.take(maxLines.getOrElse(10)).mkString("\n")
      case "last-lines"  => rec.stdout.linesIterator.toList.takeRight(maxLines.getOrElse(10)).mkString("\n")
      case "limited"     => rec.stdout.linesIterator.take(maxLines.getOrElse(30)).mkString("\n")
      case "summary"     =>
        val lines = rec.stdout.linesIterator.size
        s"Строк вывода: $lines, код завершения: ${rec.exitCode}"
      case _ => rec.stdout

    if text.nonEmpty then
      sb.append(s"```$lang\n${escapeFences(text)}\n```\n\n")


  //Экранировать ``` внутри текста, чтобы не сломать Markdown.
  private def escapeFences(text: String): String =
    text.replace("```", "`` `")