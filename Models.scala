package lab0

import java.time.LocalDateTime
import upickle.default.*

// given для LocalDateTime — ДО case-классов, иначе derives его не найдёт
given ReadWriter[LocalDateTime] =
readwriter[String].bimap[LocalDateTime](_.toString, LocalDateTime.parse)

case class CommandRecord(
                          id: Int, text: String, commandName: String, arguments: List[String],
                          cwdBefore: String, cwdAfter: String,
                          startTime: LocalDateTime, endTime: LocalDateTime,
                          exitCode: Int, stdout: String, stderr: String,
                          stepIndex: Int, included: Boolean = true, ruleId: Option[String] = None
                        ) derives ReadWriter

case class Step(
                 index: Int, title: String,
                 notes: List[String] = Nil,
                 commands: List[CommandRecord] = Nil
               ) derives ReadWriter:
  def isEmpty: Boolean = notes.isEmpty && commands.isEmpty

case class Session(
                    name: String, startTime: LocalDateTime, endTime: Option[LocalDateTime],
                    rootDirectory: String, currentDirectory: String,
                    shell: String, profile: String,
                    steps: List[Step] = Nil, excludedCount: Int = 0, status: String = "active"
                  ) derives ReadWriter