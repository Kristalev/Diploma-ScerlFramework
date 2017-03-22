package myframework
import com.ericsson.otp.erlang.{OtpMbox, OtpErlangPid, OtpErlangObject, OtpNode}


/**
  * Представление Erlang процессов в фреймворке
  *
  * @param name имя актора
  */
case class ErlangProcess(override val name:String)
  extends NotScalaActor[OtpErlangObject](name){
  /** В качетсве идентификатора используется [[com.ericsson.otp.erlang.OtpErlangPid]]
    * @see [[http://erlang.org/doc/apps/jinterface/java/com/ericsson/otp/erlang/OtpErlangPid.html]]
    */
  override type IdentifierOtherActor = OtpErlangPid

  /**
    * Внутренний Erlang процесс
    */
  val adapteeProcess:OtpMbox = ErlangNode(name)

  /** Отправка сообщения другому Erlang актору
    *
    * @param adr идентифекатор актора получателя
    * @param msg сообщение
    * @tparam T тип сообщения. Подкласс типа [[OtpErlangObject]]
    */
  override def send[T <: OtpErlangObject](adr:IdentifierOtherActor)(msg: T): Unit = adapteeProcess.send(adr,msg)

  /** Отправка сообщения другому Erlang актору
    *
    * @param aname имя актора получателя
    * @param node узел, на которам живет актор получатель
    * @param msg сообщение
    * @tparam T тип сообщения. Подкласс типа [[OtpErlangObject]]
    * @return true - если актор получатель доступен и сообщение отправлено, false - иначе
    */
  override def send[T <: OtpErlangObject](aname:String,node:String,msg: T) : Boolean =
    if(adapteeProcess.ping(node,1000)) {
      adapteeProcess.send(aname,node,msg)
      true
    } else
      false

  /**Получение сообщения
    *
    * @return [[scala.Some]] - если сообщение получено, [[scala.None]] - иначе
    */
  override def receive: Option[OtpErlangObject] = adapteeProcess.receive(1) match {
    case msg:OtpErlangObject =>
      Some(msg)
    case _=>
      None
  }

  /**
    *
    * @return идентификатор актора
    */
  def idThisActor : IdentifierOtherActor = adapteeProcess.self()
}


/**
  * Локальный Erlang узел для общения с удаленными Erlang узлами
  * @see [[http://erlang.org/doc/apps/jinterface/java/com/ericsson/otp/erlang/OtpNode.html]]
  */
object ErlangNode{
  /**
    * Внутренний Erlang узел
    */
  val localErlangNode = new OtpNode("ScalaErlangNode")

  /**
    * Фабричный метод для создания [[OtpMbox]](Erlang процесс). Используется [[ErlangProcess]]
    * @see [[http://erlang.org/doc/apps/jinterface/java/com/ericsson/otp/erlang/OtpMbox.html]]
    *
    * @param name имя создаваемого Erlang процесса
    * @return объект класса [[OtpMbox]]
    */
  def apply(name:String) = localErlangNode.createMbox(name)

  /**
    * Устанавливет Cookie для локального узла, для безопасного общения с удаленными узлами
    * @param coo задаваемый Cookie
    * @return установленный Cookie
    */
  def setCookie(coo:String) = localErlangNode.setCookie(coo)
}