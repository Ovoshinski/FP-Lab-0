package lab0

import upickle.default.*

case class InstructionOption(flag: String, description: String) derives ReadWriter
case class InstructionExample(cmd: String, desc: String) derives ReadWriter

case class Instruction(
                        name: String, category: String, description: String, usage: String,
                        options: List[InstructionOption] = Nil,
                        examples: List[InstructionExample] = Nil,
                        reportBehavior: String = ""
                      ) derives ReadWriter

case class InstructionsData(
                             categories: List[Map[String, String]],
                             commands: Map[String, Instruction]
                           ) derives ReadWriter

// ============================================================
// Справка по командам. Принимает номер или имя.
// ============================================================
object Instructions:

  // Загружаем справку ОДИН РАЗ при первом обращении.
  // Порядок: /instructions.json из ресурсов -> fallback.
  private lazy val data: InstructionsData =
    val stream = getClass.getResourceAsStream("/instructions.json")
    if stream == null then
      println("ВНИМАНИЕ: instructions.json не найден в resources.")
      println("Проверь, что файл лежит в src/main/resources/instructions.json")
      println("и что sbt пересобрал проект (Reload sbt Project).")
      fallback
    else
      try
        val text = scala.io.Source.fromInputStream(stream, "UTF-8").mkString
        val parsed = read[InstructionsData](text)
        println(s"Загружено команд из instructions.json: ${parsed.commands.size}")
        parsed
      catch
        case e: Exception =>
          println(s"Ошибка разбора instructions.json: ${e.getMessage}")
          println("Используется резервная справка.")
          fallback

  // Резервная справка (используется, если JSON не загрузился)
  private val fallback = InstructionsData(
    categories = List(Map("id" -> "all", "name" -> "Все команды")),
    commands = Map(
      "instructions" -> Instruction(
        name = "instructions", category = "service",
        description = "Справка по командам (резервная)",
        usage = "instructions [НОМЕР_ИЛИ_ИМЯ]",
        examples = List(InstructionExample("19 10", "справка по #10"))
      )
    )
  )

  // -------------------- Публичный метод --------------------

  def show(args: List[String]): Unit =
    if args.isEmpty then showList()
    else
      val arg = args.head
      // Если это число — ищем команду по номеру
      if arg.matches("\\d+") then
        val num = arg.toInt
        InstructionNumbers.map.get(num) match
          case Some(cmdName) =>
            data.commands.get(cmdName) match
              case Some(instr) => showOne(instr, Some(num))
              case None        => println(s"Команда #$num ($cmdName) не имеет справки.")
          case None =>
            println(s"Команда с номером $num не найдена.")
      else
        // Иначе — по имени
        data.commands.get(arg) match
          case Some(instr) => showOne(instr, InstructionNumbers.reverseMap.get(arg))
          case None        => println(s"Команда не найдена: $arg")

  // -------------------- Приватные методы --------------------

  // Список всех команд с номерами
  private def showList(): Unit =
    println()
    println("Доступные команды (можно вводить номер или имя):")
    println()

    // Сортируем по номеру
    val sorted = data.commands.values.toList.sortBy { c =>
      InstructionNumbers.reverseMap.getOrElse(c.name, 999)
    }

    sorted.foreach { c =>
      val num = InstructionNumbers.reverseMap.get(c.name) match
        case Some(n) => f"[$n%2d]"
        case None    => "[  ]"
      println(f"  $num ${c.name}%-14s - ${c.description}")
    }

  // Описание одной команды
  private def showOne(instr: Instruction, num: Option[Int]): Unit =
    val numStr = num.map(n => s" [#$n]").getOrElse("")
    println()
    println(s"${instr.name}$numStr - ${instr.description}")
    println()
    println("Использование:")
    println(s"  ${instr.usage}")
    println()

    if instr.options.nonEmpty then
      println("Опции:")
      instr.options.foreach(o => println(f"  ${o.flag}%-6s ${o.description}"))
      println()

    if instr.examples.nonEmpty then
      println("Примеры:")
      instr.examples.foreach(e => println(f"  ${e.cmd}%-30s # ${e.desc}"))
      println()

    if instr.reportBehavior.nonEmpty then
      println(s"В отчёте: ${instr.reportBehavior}")