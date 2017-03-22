package bigdata

import akka.actor.{Actor, Props}

/**
  * Актор коллектор, уменшающий поток данных к главному процессу.
  * Для получения данных создает себе помошника [[ErlangListener]]
  * ==Принимаемые сообщения==
  * - Int - числа для поиска минимума<br>
  *
  * ==Отправлятемые сообщения==
  * - [[DataPart]] - сообщение с набором данных для обработки <br>
  *
  * '''Общается с:''' [[ErlangListener]], [[MainConsumer]]
  * @param name имя актора
  */
class GuardCollector(name:String) extends Actor{
  /**Множество чисел для обработки*/
  private var setInt: Set[Int] = Set.empty

  /**Создание акотра слушателя [[ErlangListener]]*/
  private val listener = context.actorOf(ErlangListener.props(name))
  listener ! "get"
  override def receive: Receive = {
    case n:Int =>
      setInt+=n
      if(setInt.size == 5000){
        context.parent ! DataPart(setInt)
        setInt = Set.empty
      }
  }
}

/**Объект-путник, служащий поставщиком фабричного метода для создания конфигурация для актора [[GuardCollector]]*/
object GuardCollector{
  def props(name:String) = Props(new GuardCollector(name))
}
