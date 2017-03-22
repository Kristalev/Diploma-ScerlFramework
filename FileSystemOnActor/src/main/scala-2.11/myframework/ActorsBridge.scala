package myframework

/**
  * Абстрактный класс для акторов, не являющихся потомками [[akka.actor.Actor]]
  * @see [[http://doc.akka.io/api/akka/2.4.7/?_ga=1.40424290.1913219468.1458323745#akka.actor.Actor]]
  *
  * @param name имя актора
  * @tparam TypeMessage суперкласс типов сообщений, с которыми работает актор
  */
abstract class NotScalaActor[TypeMessage](val name:String){

  /** Псевдоним для типов идентификаторов акторов*/
  type IdentifierOtherActor

  /**
    * Метод отправки сообщений другим акторам
    *
    * @param adr идентифекатор актора получателя
    * @param msg сообщение
    * @tparam T тип сообщения. Подкласс типа [[TypeMessage]]
    */
  def send[T <: TypeMessage](adr: IdentifierOtherActor)(msg: T) : Unit

  /**
    * Метод отправки сообщений другим акторам
    *
    * @param aname имя актора получателя
    * @param node узел, на которам живет актор получатель
    * @param msg сообщение
    * @tparam T тип сообщения. Подкласс типа [[TypeMessage]]
    * @return true - если актор получатель доступен и сообщение отправлено, false - иначе
    */
  def send[T <: TypeMessage](aname:String,node:String, msg: T) : Boolean

  /**
    * Метод получения сообщения
    *
    * @return [[scala.Some]] - если сообщение получено, [[scala.None]] - иначе
    */
  def receive : Option[TypeMessage]

  /**
    *
    * @return идентификатор актора
    */
  def idThisActor : IdentifierOtherActor
}

