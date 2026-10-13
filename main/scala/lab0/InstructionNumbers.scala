package lab0

//Реестр номеров команд
// 0 — выход
// 1-9 — файловые команды
// 10-19 — команды для работы с отчётом
object InstructionNumbers:

  val map: Map[Int, String] = Map(
    0  -> "exit",

    1  -> "cd",
    2  -> "pwd",
    3  -> "ls",
    4  -> "mkdir",
    5  -> "touch",
    6  -> "cat",
    7  -> "cp",
    8  -> "mv",
    9  -> "rm",

    10 -> "starts",
    11 -> "prootd",
    12 -> "mstep",
    13 -> "mnote",
    14 -> "mskip",
    15 -> "mstatus",
    16 -> "mpreview",
    17 -> "info",
    18 -> "instructions",
    19 -> "ends"
  )

  val reverseMap: Map[String, Int] = map.map(_.swap)


  //Переводит ввод в (имя_команды, аргументы)
  def resolve(input: String): (String, List[String]) =
    val (first, rest) = CommandParser.parse(input)

    if first.matches("\\d+") then
      map.get(first.toInt) match
        case Some("mv") =>
          rest.headOption match
            case Some("rename") | Some("move") => ("mv", rest.tail)
            case _ => ("mv", rest)
        case Some(cmd) => (cmd, rest)
        case None      => (first, rest)
    else
      first match
        case "rename" | "move" => ("mv", rest)
        case "mv" =>
          rest.headOption match
            case Some("rename") | Some("move") => ("mv", rest.tail)
            case _ => ("mv", rest)
        case _ => (first, rest)