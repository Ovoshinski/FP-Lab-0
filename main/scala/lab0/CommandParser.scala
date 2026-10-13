package lab0

//Разбор строки на имя команды и список аргументов
object CommandParser:

  def parse(line: String): (String, List[String]) =
    val tokens = tokenize(line.trim)
    if tokens.isEmpty then ("", Nil)
    else (tokens.head, tokens.tail)


  //Посимвольно разбирает строку, отслеживает кавычки
  private def tokenize(s: String): List[String] =
    val result = scala.collection.mutable.ListBuffer[String]()
    val current = new StringBuilder
    var inQuotes = false
    var quoteChar = ' '
    var i = 0

    while i < s.length do
      val c = s.charAt(i)

      if inQuotes then
        //Внутри кавычек — закрывающая кавычка или символ
        if c == quoteChar then inQuotes = false
        else current.append(c)
      else if c == '"' || c == '\'' then
        //Открываем кавычки
        inQuotes = true
        quoteChar = c
      else if c.isWhitespace then
        //Пробел — конец текущего токена
        if current.nonEmpty then
          result += current.toString
          current.clear()
      else
        current.append(c)

      i += 1

    if current.nonEmpty then result += current.toString
    result.toList