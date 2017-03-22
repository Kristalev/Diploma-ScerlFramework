package bigdata

import akka.actor.Actor

/**
  * Актор, который ищет в небольшой порции данных min и отправляте его [[MainConsumer]]
  *
  * ==Принимаемые сообщения==
  * - [[DataPart]] - сообщение с данными для обработки<br>
  *
  * ==Отправляемые сообщения==
  * - [[LocalMin]] - сообщение с минимумом в полученной порции данных<br>
  *
  *    '''Общается с:''' [[MainConsumer]]
  */
class MinSearcher extends Actor{
  override def receive: Receive = {
    case DataPart(setInt) =>
      context.parent ! LocalMin(setInt.min)
      context.stop(self)
  }
}
