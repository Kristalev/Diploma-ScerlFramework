package myframework
import akka.actor.Actor
import com.ericsson.otp.erlang._

/**Абстрактный класс, потомки которого одновременно и Erlang процессы и Scala акторы.
  *==Принимаемые сообщения==
  * - [[ErlangMessage]] - сообщение для обработки методом [[erlangBehavior()]];<br>
  * - [[ScalaMessage]] - cообщение для обработки методом [[scalaBehavior()]];
  *
  * @see [[http://doc.akka.io/api/akka/2.4.7/?_ga=1.40424290.1913219468.1458323745#akka.actor.Actor]]
  * @param name имя актора
  */
abstract class ErlangAdapteeActor(val name:String) extends Actor{
  case object ContinueGetMessage

  /**Внутренний Erlang процесс для общения с другими Erlang процессами*/
  protected val innerErlProc = ErlangProcess(name)

  /**
    *
    *  получение сообщений, для обработки
    */
  override def receive: Receive = {
    case ContinueGetMessage =>
      val o = innerErlProc.receive
      val mess = o.getOrElse(new OtpErlangAtom("nothing"))
      mess match {
        case at:OtpErlangAtom if at.toString == "nothing" =>
          self ! ContinueGetMessage
        case _=>
          self ! ErlangMessage(mess)
      }
    case  ErlangMessage(msg) =>
      erlangBehavior(msg)
    case ScalaMessage(msg)=>
      scalaBehavior(msg)

  }

  /**
    * Поведение данного актора, как Erlang процесса
    * @param msg обрабатываемое сообщение
    * @tparam U тип сообщения
    */
  def erlangBehavior[U<:OtpErlangObject](msg:U) : Unit

  /**Поведение данного актора, как Scala актора
    *
    * @param msg обрабатываемое сообщение
    * @tparam U тип сообщения
    */
  def scalaBehavior[U](msg:U) : Unit

  protected def russianChars(fileName:OtpErlangObject):String = fileName match {
    case list:OtpErlangList =>
      list.elements().map(_.asInstanceOf[OtpErlangLong]).map(_.charValue()).mkString
    case str:OtpErlangString =>
      str.toString.tail.dropRight(1)
  }
}
