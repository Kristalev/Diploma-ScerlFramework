package filesystem

import akka.actor.{Actor, ActorRef, Props, ReceiveTimeout}

import scala.concurrent.duration._

/**
  * Клиенский актор. Служит прослойкой между интерфейсом и системой акторов
  *
  * Данный актор имеет два состояния: инициализация системы и работы с системой
  *
  * ==Принимаемые сообщения==
  *
  * ===Инициализация системы===
  * - scala.Tuple2("init", Set[String]) - сообщение для начала инициализации системы.<br>
  * - [[InitDone]] - сообщение об успешной инициализации системы<br>
  * - [[InitError]] - сообщение об неполадках во время инициализации <br>
  * - [[ReceiveTimeout]] - сообщение о завершении времени ожиданя<br>
  *
  * ===Работа системы ===
  * - [[FileDeleted]] - сообщение об удалении файла<br>
  * - [[FileRenamed]] - сообщение об переименовании файла<br>
  * - [[FileCopied]] - сообщение об копировании файла<br>
  * - [[CommandToFS]] - сообщение с командами для файловой системы<br>
  */
class ClientForGUI extends Actor{
  val mainFS = context.actorOf(Props[MainFS],"mainFS")
  context.watch(mainFS)
  var lastSender:ActorRef = null
  override def receive: Receive ={
    case ("init",setNodes:scala.collection.mutable.Set[String]) =>
      context.setReceiveTimeout(10 second)
      mainFS ! InitCommand(setNodes.toSet)
      lastSender = sender()
    case InitDone(setNode)=>
      context.setReceiveTimeout(Duration.Undefined)
      val listFileOnNode = setNode
        .map(_.nodeName)
        .zip(setNode
          .map(_.mapProc)
          .map(_.keys))
        .toList
      lastSender ! listFileOnNode
      context.become(workerWithFS)
    case InitError =>
      lastSender ! "Error init"
    case ReceiveTimeout =>
      lastSender ! "Receive Timeout in init"
  }



  private var commands: Set[CommandToNode] = Set.empty
  def workerWithFS:Receive = {
    case ("delete", node:String, file:String) =>
      val delFile = DeleteFile(file)
      val comandToNode = CommandToNode(node, delFile)
      commands += comandToNode
    case ("rename",node:String,oldFileName:String,newFileName:String) =>
      val renameFile = RenameFile(oldFileName, newFileName)
      val comandToNode = CommandToNode(node, renameFile)
      commands += comandToNode
    case ("copy",node:String,oldFilePlace:String,newFilePlace:String) =>
      val copyFile = CopyFile(oldFilePlace, newFilePlace)
      val comandToNode = CommandToNode(node, copyFile)
      commands += comandToNode
    case "exe" =>
      lastSender = sender()
      mainFS ! CommandToFS(commands)
      commands = Set.empty
    case (str:String, node:String, map:Map[String,_])=>
      lastSender ! str
      lastSender ! (node,map.keys.toList)
  }

}
