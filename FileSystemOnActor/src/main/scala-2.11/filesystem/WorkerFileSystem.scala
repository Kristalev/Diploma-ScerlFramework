package filesystem

import akka.actor.Props
import com.ericsson.otp.erlang._
import myframework.ErlangAdapteeActor

/**Объект-путник, служащий поставщиком фабричного метода для создания конфигурация для актора [[FileSystemProcess]]*/
object FileSystemProcess {
  def props(name: String) = Props(new FileSystemProcess(name))
}

/**Актор - обработчик команды файловой системы
  * ==Принимаемые сообщения==
  *
  * ===Scala сообщения===
  * ''pidFile - идентификатор процесса, обрабатывающего файл''<br>
  * - scala.Tuple2(pidFile: OtpErlangPid, "delete") - команда на удаление файла<br>
  * - scala.Tuple3(pidFile: OtpErlangPid, newName:String, "rename") - команда на переименование файла<br>
  * - scala.Tuple3(pidFile: OtpErlangPid, copyName:String, "copy") - команда на копирование файла<br>
  *
  * ===Erlang сообщения===
  * - OtpErlangTuple("deleted", node, file) - ответ об удалении файла<br>
  * - OtpErlangTuple("renamed", node, oldFileName, newFileName) - ответ о переименовании файла<br>
  * - OtpErlangTuple("copied", node, oldFileName, newFileName, pidNewFile) - ответ о копировании файла<br>
  *
  *
  * @param name имя актора
  */
class FileSystemProcess(name: String) extends ErlangAdapteeActor(name) {


  /**@inheritdoc
    *
    * @param msg обрабатываемое сообщение
    * @tparam U тип сообщения
    */
  override def erlangBehavior[U <: OtpErlangObject](msg: U): Unit = msg match {
    case tuple: OtpErlangTuple =>
      tuple.elementAt(0).toString match {
        case "deleted" =>
          val file = russianChars(tuple.elementAt(2))
          val mes = FileDeleted(tuple.elementAt(1).toString.tail.dropRight(1),file)
          context.parent ! mes
          context.stop(self)
        case "renamed" =>
          val oldfile = russianChars(tuple.elementAt(2))
          val newfile = russianChars(tuple.elementAt(3))
          val mes = FileRenamed(tuple.elementAt(1).toString.tail.dropRight(1),
            oldfile,newfile)
          context.parent ! mes
          context.stop(self)
        case "copied" =>
          val oldfile = russianChars(tuple.elementAt(2))
          val newfile = russianChars(tuple.elementAt(3))
          val mes = FileCopied(tuple.elementAt(1).toString.tail.dropRight(1),
            oldfile,newfile,
            tuple.elementAt(4).asInstanceOf[OtpErlangPid])
          context.parent ! mes
          context.stop(self)
      }
  }

  /**@inheritdoc
    *
    * @param msg обрабатываемое сообщение
    * @tparam U тип сообщения
    */
  override def scalaBehavior[U](msg: U): Unit = msg match {
    case (pidFile: OtpErlangPid, "delete") =>
      val msg = new Array[OtpErlangObject](2)
      msg(0) = new OtpErlangAtom("delete")
      msg(1) = innerErlProc.idThisActor
      innerErlProc.send(pidFile)(new OtpErlangTuple(msg))
      self ! ContinueGetMessage
    case (pidFile: OtpErlangPid, newName:String, "rename") =>
      val msg = new Array[OtpErlangObject](3)
      msg(0) = new OtpErlangAtom("rename")
      msg(1) = new OtpErlangString(newName)
      msg(2) = innerErlProc.idThisActor
      innerErlProc.send(pidFile)(new OtpErlangTuple(msg))
      self ! ContinueGetMessage
    case (pidFile: OtpErlangPid, copyName:String, "copy") =>
      val msg = new Array[OtpErlangObject](3)
      msg(0) = new OtpErlangAtom("copy")
      msg(1) = new OtpErlangString(copyName)
      msg(2) = innerErlProc.idThisActor
      innerErlProc.send(pidFile)(new OtpErlangTuple(msg))
      self ! ContinueGetMessage
  }
}
