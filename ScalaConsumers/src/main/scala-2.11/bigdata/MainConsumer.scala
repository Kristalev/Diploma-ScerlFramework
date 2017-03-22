package bigdata

import akka.actor.{Actor, Props, Terminated}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Главный актор в данной системе.
  * Для получения данных этот актор сосздает [[GuardCollector]], а для обработки полученных чисел
  * создает [[MinSearcher]]
  * В своем состоянии хранит искомые величины.
  *
  * ==Принимаемые сообщения==
  * - "Start system" - сообщение о запуске системы<br>
  * - [[DataPart]] - сообщение с данными для обработки<br>
  * - [[Terminated]] - сообщение об остановке одного из [[GuardCollector]]<br>
  * - [[LocalMin]] - сообщение с минимумом в некоторой порции данных<br>
  * - "Get min" - запрос состояния актора<br>
  *
  * ==Отправляемые сообщения==
  * - "get" - сообщение о начале получения данных<br>
  * - [[DataPart]] - сообщение с данными для обработки<br>
  *
  *   '''Общается с:''' [[GuardCollector]], [[MinSearcher]], интерфейсом пользователя.
  */
class MainConsumer extends Actor{
  /**Глобальный минимумом*/
  private var mainMin = 99999999
  /**Последний полученный минимум*/
  private var localMin = mainMin
  override def receive: Receive = {
    case "Start system" =>
      startAllCollectors(5)
      println("System start!")
    case Terminated(worker) =>
      println(worker.toString() + " : Terminated")
    case DataPart(setInt) =>
      Future{searchMin(setInt)}
      println("Get bigdata.DataPart from: " + sender().path)
    case LocalMin(min) =>
      localMin = min
      if(min < mainMin)
        mainMin = min
      println("Get new mins: " + localMin)
    case "Get min" =>
      sender() ! (mainMin,localMin)
  }

  /**
    *Метод для запуска [[GuardCollector]]
    * @param count количество запускаемых [[GuardCollector]]
    */
  def startAllCollectors(count:Int):Unit = {
    if (count > 0) {
      val collector = context.actorOf(GuardCollector.props("collector" + count),"collector" + count)
      context.watch(collector)
      collector ! "get"
      startAllCollectors(count-1)
    }
  }

  /**
    * Делит данные на небольшие порции для обработки, создает [[MinSearcher]] и отправлет им эти порции
    * @param setInt набор данных для обработки
    */
  def searchMin(setInt:Set[Int]) ={
    def helpFun(tailSet:List[Int]):Unit= tailSet match {
      case Nil => Unit
      case x:List[Int] =>
        val k = x.splitAt(1000)
        val searcher = context.actorOf(Props[MinSearcher])
        searcher ! DataPart(k._1.toSet)
        helpFun(k._2)
    }
    helpFun(setInt.toList)
  }
}
