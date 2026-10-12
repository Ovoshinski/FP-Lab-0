package lab0

// ============================================================
// Транслятор POSIX-команд в аналоги Windows.
// На Linux/macOS/macOS ничего не меняет.
// На Windows 11 переводит ls, cat, mkdir, touch, rm, cp, mv,
// pwd, grep в команды cmd.exe (dir, type, mkdir, del, copy,
// move, cd, findstr).
// ============================================================
object CommandTranslator:

  // Определяем ОС
  val isWindows: Boolean =
    System.getProperty("os.name").toLowerCase.contains("win")

  // Вернуть (исполняемый_файл, аргументы) — уже под текущую ОС
  def translate(cmd: String, args: List[String]): (String, List[String]) =
    if !isWindows then (cmd, args)
    else
      cmd match
        case "ls"    => translateLs(args)
        case "cat"   => ("cmd.exe", List("/c", "type") ++ args)
        case "pwd"   => ("cmd.exe", List("/c", "cd"))
        case "mkdir" => ("cmd.exe", List("/c", "mkdir") ++ args)
        case "touch" => translateTouch(args)
        case "rm"    => translateRm(args)
        case "cp"    => translateCp(args)
        case "mv"    => ("cmd.exe", List("/c", "move") ++ args.filterNot(_.startsWith("-")))
        case "grep"  => ("cmd.exe", List("/c", "findstr") ++ args)
        case _       => (cmd, args)

  // ls -> dir (с флагом /a, если есть -a)
  private def translateLs(args: List[String]): (String, List[String]) =
    val showAll = args.exists(a => a.startsWith("-") && a.contains("a"))
    val paths   = args.filterNot(_.startsWith("-"))
    val flags   = if showAll then List("/a") else Nil
    ("cmd.exe", List("/c", "dir") ++ flags ++ paths)

  // touch -> cmd /c "type nul > file"
  private def translateTouch(args: List[String]): (String, List[String]) =
    args.headOption match
      case Some(f) => ("cmd.exe", List("/c", s"type nul > $f"))
      case None    => ("cmd.exe", List("/c", "type", "nul"))

  // rm -> del (или rmdir /s /q для -r)
  private def translateRm(args: List[String]): (String, List[String]) =
    val recursive = args.exists(a => a.startsWith("-") && a.contains("r"))
    val files = args.filterNot(_.startsWith("-"))
    files.headOption match
      case Some(f) if recursive => ("cmd.exe", List("/c", "rmdir", "/s", "/q", f))
      case Some(f)              => ("cmd.exe", List("/c", "del", f))
      case None                 => ("cmd.exe", List("/c", "del"))

  // cp -> copy
  private def translateCp(args: List[String]): (String, List[String]) =
    val clean = args.filterNot(_.startsWith("-"))
    ("cmd.exe", List("/c", "copy") ++ clean)