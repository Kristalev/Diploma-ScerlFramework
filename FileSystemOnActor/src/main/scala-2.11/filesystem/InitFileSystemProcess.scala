package filesystem

import akka.actor.Props
import com.ericsson.otp.erlang._
import myframework.ErlangAdapteeActor


/**Актор - инициализатор файловой системы
  * ==Принимаемые сообщения==
  *
  * ===Scala сообщения===
  * - Set[String]- узлы для инициализации<br>
  *
  * ===Erlang сообщения===
  * - OtpErlangTuple(node,mapProc) - ответ от инициализированного Erlang узла<br>
  *
  *
  * @param name имя актора
  */
class InitFileSystemProcess(name: String) extends ErlangAdapteeActor(name) {
  /**Множество ожидаемых подключения узлов*/
  private var setNodes: Set[String] = Set.empty

  /**@inheritdoc
    *
    * @param msg обрабатываемое сообщение
    * @tparam U тип сообщения
    */
  override def erlangBehavior[U <: OtpErlangObject](msg: U): Unit = msg match {
    case m: OtpErlangTuple =>
      m.elementAt(1) match {
        case mapFiles: OtpErlangMap =>
          val f = russianChars _
          val scalaMapFiles: Map[String, OtpErlangPid] = mapFiles
            .keys() //берем все названия файлов
            .map(f)
            .zip(mapFiles.values()) //все в кортеж
            .groupBy(_._1)
            .map { case (k, v) => (k, v.map(_._2).apply(0).asInstanceOf[OtpErlangPid]) }
          context.parent ! ConnectedNode(m.elementAt(0).toString.tail.dropRight(1), scalaMapFiles)
          setNodes -= m.elementAt(0).toString.tail.dropRight(1)
          if (setNodes.isEmpty)
            context.stop(self)
          else
            self ! ContinueGetMessage
      }
  }

  /**@inheritdoc
    *
    * @param msg обрабатываемое сообщение
    * @tparam U тип сообщения
    */
  override def scalaBehavior[U](msg: U): Unit = msg match {
    case nodeList: Set[String] =>
      nodeList foreach (node => startFSonNode(node))
      self ! ContinueGetMessage
  }

  /**Подключение узла к файловой системе
    *
    * @param node подключаемый узел
    */
  private def startFSonNode(node: String) = {
    val msg = new Array[OtpErlangObject](2)
    msg(0) = new OtpErlangAtom("getFS")
    msg(1) = innerErlProc.idThisActor
    if (!innerErlProc.send("getFS", node, new OtpErlangTuple(msg)))
      context.parent ! NodeNotConnect(node)
    else setNodes += node
  }
}

/**
  * Объект-путник, служащий поставщиком фабричного метода для создания конфигурация для актора [[InitFileSystemProcess]]
  */
object InitFileSystemProcess {
  def props(name: String): Props = Props(new InitFileSystemProcess(name))
}