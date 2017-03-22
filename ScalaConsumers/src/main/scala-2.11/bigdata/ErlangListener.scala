package bigdata

import akka.actor.{Actor, Props}
import com.ericsson.otp.erlang.{OtpErlangAtom, OtpErlangLong}
import myframework.ErlangProcess

import scala.annotation.tailrec

/**
  * Актор, принимающий сообщения от Erlang процессов и отправляющий своему [[GuardCollector]]
  *  ==Принимаемые сообщения==
  *
  *===Scala сообщения===
  * - "get" - сообщение о начеле получении всех сообщений от Erlang<br>
  *
  * ===Erlang сообщения===
  * - OtpErlangLong - число, сгенерированное Erlang процессом<br>
  *
  * ==Отправлятемые сообщения==
  *
  * ===Scala сообщения===
  * - Int - полученное число для обработки<br>
  *
  *  '''Общается с:''' [[GuardCollector]], удаленными Erlang процессами.
  * @param name имя актора
  */
class ErlangListener(name:String) extends Actor{
  /**Внутренний Erlang процесс для общения с другими Erlang процессами*/
  private val innerErlProc = ErlangProcess(name)
  override def receive: Receive ={
    case "get" =>
      listenErlang()
  }

  /**
    * Получение сообщений от erlang процессов и отправка их своему [[GuardCollector]]
    */
  @tailrec
  private def listenErlang(): Unit ={
    val o = innerErlProc.receive
    val mess = o.getOrElse(new OtpErlangAtom("nothing"))
    mess match {
      case num:OtpErlangLong =>
        context.parent ! num.intValue()
      case _ =>
    }
    listenErlang()
  }
}

/**Объект-путник, служащий поставщиком фабричного метода для создания конфигурация для актора [[ErlangListener]]*/
object ErlangListener{
  def props(name:String) = Props(new ErlangListener(name))
}
