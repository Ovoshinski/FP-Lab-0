package lab0

// ============================================================
// Реестр номеров команд.
// Позволяет вводить "0", "1", ..., "19" вместо имён команд.
// Например, "0" = ends, "18" = instructions.
// Если пользователь вводит "18 10" — это "справка про команду #10".
// ============================================================
object InstructionNumbers:

  // Соответствие: номер -> имя команды
  val map: Map[Int, String] = Map(
    0  -> "ends",
    1  -> "starts",
    2  -> "prootd",
    3  -> "mstep",
    4  -> "mnote",
    5  -> "mskip",
    6  -> "mstatus",
    7  -> "mpreview",
    8  -> "cd",
    9  -> "pwd",
    10 -> "ls",
    11 -> "mkdir",
    12 -> "touch",
    13 -> "cat",
    14 -> "cp",
    15 -> "mv",
    16 -> "rm",
    17 -> "info",
    18 -> "instructions",
    19 -> "exit"
  )

  // Обратное соответствие: имя -> номер
  val reverseMap: Map[String, Int] = map.map(_.swap)

  // Разобрать ввод: если первый токен — число, заменить его на имя команды.
  def resolve(input: String): (String, List[String]) =
    val (first, rest) = CommandParser.parse(input)
    if first.matches("\\d+") then
      map.get(first.toInt) match
        case Some(cmd) => (cmd, rest)
        case None      => (first, rest)
    else
      (first, rest)