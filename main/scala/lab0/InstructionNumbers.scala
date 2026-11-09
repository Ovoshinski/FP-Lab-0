package lab0

//Реестр номеров команд
object InstructionNumbers:

  val map: Map[Int, String] = Map(
    0  -> "exit",

    //Файловые команды
    1  -> "cd",
    2  -> "pwd",
    3  -> "dir",
    4  -> "mkdir",
    5  -> "type",
    6  -> "copy",
    7  -> "move",
    8  -> "del",

    //Отчетные команды
    9  -> "starts",
    10 -> "prootd",
    11 -> "mstep",
    12 -> "mnote",
    13 -> "mskip",
    14 -> "mstatus",
    15 -> "mpreview",
    16 -> "info",
    17 -> "instructions",
    18 -> "ends"
  )

  val reverseMap: Map[String, Int] = map.map(_.swap)

  def resolve(input: String): (String, List[String]) =
    val (first, rest) = CommandParser.parse(input)
    if first.matches("\\d+") then
      map.get(first.toInt) match
        case Some(cmd) => (cmd, rest)
        case None      => (first, rest)
    else
      (first, rest)