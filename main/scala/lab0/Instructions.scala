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


//Справка по командам. Загружает JSON из resources, кэширует
object Instructions:

  private lazy val data: InstructionsData =
    val stream = getClass.getResourceAsStream("/instructions.json")
    if stream == null then
      println("ВНИМАНИЕ: instructions.json не найден в resources.")
      fallback
    else
      try
        val text = scala.io.Source.fromInputStream(stream, "UTF-8").mkString
        read[InstructionsData](text)
      catch
        case e: Exception =>
          println(s"Ошибка разбора instructions.json: ${e.getMessage}")
          fallback


  private val fallback = InstructionsData(
    categories = List(Map("id" -> "all", "name" -> "Все команды")),
    commands = Map(
      "instructions" -> Instruction(
        name = "instructions", category = "report",
        description = "Справка по командам",
        usage = "instructions [номер_или_имя]"
      )
    )
  )


  //Показ справки. Без аргументов — список, с аргументом — одна команда
  def show(args: List[String]): Unit =
    if args.isEmpty then showList()
    else
      val arg = args.head
      if arg.matches("\\d+") then
        val num = arg.toInt
        InstructionNumbers.map.get(num) match
          case Some(cmdName) =>
            data.commands.get(cmdName) match
              case Some(instr) => showOne(instr, Some(num))
              case None        => println(s"Команда #$num ($cmdName) не имеет справки."); println()
          case None =>
            println(s"Команда с номером $num не найдена."); println()
      else
        data.commands.get(arg) match
          case Some(instr) => showOne(instr, InstructionNumbers.reverseMap.get(arg))
          case None        => println(s"Команда не найдена: $arg"); println()


  //Список всех команд, разбитый на два типа
  private def showList(): Unit =
    val filesCmds  = data.commands.values.toList.filter(_.category == "files")
      .sortBy(c => InstructionNumbers.reverseMap.getOrElse(c.name, 999))
    val reportCmds = data.commands.values.toList.filter(_.category == "report")
      .sortBy(c => InstructionNumbers.reverseMap.getOrElse(c.name, 999))

    println("Команды для работы с файлами:")
    filesCmds.foreach(printCmdLine)
    println()
    println("Команды для работы с отчётом:")
    reportCmds.foreach(printCmdLine)
    println()
    println("Введите номер или имя команды.")
    println("Для подробной справки: 18 <номер_или_имя>")
    println()


  private def printCmdLine(c: Instruction): Unit =
    val num = InstructionNumbers.reverseMap.get(c.name) match
      case Some(n) => f"[$n%2d]"
      case None    => "[  ]"
    println(f"  $num  ${c.name}%-14s - ${c.description}")


  //Подробная справка по одной команде
  private def showOne(instr: Instruction, num: Option[Int]): Unit =
    val numStr = num.map(n => s" [#$n]").getOrElse("")
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
      instr.examples.foreach(e => println(f"  ${e.cmd}%-40s # ${e.desc}"))
      println()

    if instr.reportBehavior.nonEmpty then
      println(s"Что будет написано в отчёте: ${instr.reportBehavior}")
      println()