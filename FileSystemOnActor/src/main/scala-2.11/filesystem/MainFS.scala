package filesystem

import akka.actor.{ReceiveTimeout, Terminated, Actor}
import com.ericsson.otp.erlang.OtpErlangPid
import myframework.{ScalaMessage, ErlangNode}


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * Главный актор в файловой системе. Данный актор инициализирует систему и обрабатывает запросы к ней.
  *
  * Данный актор имеет два состояния: инициализация системы и работы с системой
  *
  * ==Принимаемые сообщения==
  *
  * ===Инициализация системы===
  * - [[InitCommand]] - сообщение для начала инициализации системы.<br>
  * - [[ConnectedNode]] - сообщение о подключенном к системе узле<br>
  * - [[NodeNotConnect]] - сообщение о неподключенном к системе узле <br>
  * - [[Terminated]] - сообщение об остановке актора, за которым следит данный актор<br>
  * - [[ReceiveTimeout]] - сообщение о завершении времени ожиданя<br>
  *
  * ===Работа системы ===
  * - [[FileDeleted]] - сообщение об удалении файла<br>
  * - [[FileRenamed]] - сообщение о переименовании файла<br>
  * - [[FileCopied]] - сообщение о копировании файла<br>
  * - [[CommandToFS]] - сообщение с командами для файловой системы<br>
  */
class MainFS extends Actor {

  /**Карта процессов файловой системы, где key- имя файла, value - pid процесса обслуживающего файл*/
  private var filesystem = Map.empty[String, Map[String, OtpErlangPid]].withDefaultValue(Map.empty)

  /**Ссылка на инициализирующий актор [[InitFileSystemProcess]]*/
  private val initProc = context.actorOf(InitFileSystemProcess.props("InitProcess"), "InitProcess")
  context.watch(initProc)
  /**Количество текущих работающих команд*/
  private var countWorkers = 0
  /**Ссылка на актора - клиента системы*/
  private var lastSender = Actor.noSender

  /**Множество подключенных узлов*/
  private var setConnectedNode: Set[ConnectedNode] = Set.empty


  /**
    *  Состояние инициализации системы
    *
    */
  override def receive: Receive = {
    case InitCommand(nodeList) =>
      context.setReceiveTimeout(7 second)
      initProc ! ScalaMessage(nodeList)
      lastSender = sender()
    case conNode @ ConnectedNode(nameNode, mapProc) =>
      //по хорошему проверка, что таких нодов еще нет в нашей карте
      filesystem += nameNode -> mapProc
      println(nameNode + " is connected")
      setConnectedNode += conNode
    case NodeNotConnect(node) =>
      println(node + " isn't connected")
    case Terminated(inpr) if inpr equals initProc =>
      context.setReceiveTimeout(Duration.Undefined)
      println("Initialization complite!")
      lastSender ! InitDone(setConnectedNode)
      context.become(workWithFS)
    case ReceiveTimeout  =>
      context.setReceiveTimeout(Duration.Undefined)
      if (setConnectedNode.nonEmpty){
        lastSender ! InitDone(setConnectedNode)
        context.become(workWithFS)
      }
      else{
        lastSender ! InitError
        context.stop(self)
      }


  }

  /**
    *
    *  Состояние работы с системой
    */
  private def workWithFS:Receive = {
    case FileDeleted(node, nameFile) =>
      if (filesystem(node).nonEmpty) {
        val mapFile = filesystem(node)
        if (mapFile.contains(nameFile)) {
          val newMap = mapFile - nameFile
          filesystem -= node
          filesystem += node -> newMap
          val str = node + ":" + nameFile + " was deleted!"
          println(str)
          lastSender ! (str, node, newMap)
          countWorkers = countWorkers - 1
        }
      }
    case FileRenamed(node,oldFile,newFile) =>
      if (filesystem(node).nonEmpty) {
        val mapFile = filesystem(node)
        if (mapFile.contains(oldFile)) {
          val pidOldFile = mapFile(oldFile)
          val helpMap =  mapFile - oldFile
          val newMap = helpMap + (newFile->pidOldFile)
          filesystem -= node
          filesystem += node -> newMap
          val str = node + ":" + oldFile + " was renamed to " + newFile +" !"
          println(str)
          lastSender ! (str, node, newMap)
          countWorkers = countWorkers - 1
        }
      }
    case FileCopied(node,originalFile,newFile, pidNewFile) =>
      if (filesystem(node).nonEmpty) {
        val mapFile = filesystem(node)
        if (!mapFile.contains(newFile)) {
          val newMap = mapFile + (newFile->pidNewFile)
          filesystem -= node
          filesystem += node -> newMap
          val str = node + ":" + originalFile + " was copied to " + newFile +" !"
          println(str)
          lastSender ! (str, node, newMap)
          countWorkers = countWorkers - 1
        }
      }
    case CommandToFS(commands) =>
      lastSender = sender()
      commands.foreach (com => Future(exeCommand(com)) )
  }

  /**
    * Запускает акторы, обрабатывающие команды к файловой системе
    *
    * @param command список команд к файловой системе
    */
  private def exeCommand(command:CommandToNode) = {
    val CommandToNode(node, commandToFile) = command
    if (!filesystem(node).isEmpty) {
      if (ErlangNode.localErlangNode.ping(node, 1000)) {
        val mapPR = filesystem(node)
        commandToFile match {
          case DeleteFile(nameFile) =>
            val pid = mapPR get nameFile
            pid match {
              case Some(x) =>
                val worker = context.actorOf(FileSystemProcess.props("delete:" + countWorkers), "delete:" + countWorkers)
                worker ! ScalaMessage((x, "delete"))
                countWorkers = countWorkers + 1
              case None =>
                println("File: " + node + ":" + nameFile + " not found")
            }
          case RenameFile(oldFile,newFile) =>
            val pid = mapPR get oldFile
            pid match {
              case Some(x) =>
                val worker = context.actorOf(FileSystemProcess.props("rename:" + countWorkers), "rename:" + countWorkers)
                worker ! ScalaMessage((x,newFile, "rename"))
                countWorkers = countWorkers + 1
              case None =>
                println("File: " + node + ":" + oldFile + " not found")
            }
          case CopyFile(nameFile,copyName) =>
            val pid = mapPR get nameFile
            pid match {
              case Some(x) =>
                val worker = context.actorOf(FileSystemProcess.props("copy:" +countWorkers), "copy:" + countWorkers)
                worker ! ScalaMessage((x,copyName, "copy"))
                countWorkers = countWorkers + 1
              case None =>
                println("File: " + node + ":" + nameFile + " not found")
            }

        }
      } else println(node + " not connected")
    } else println(node + " - node not found!")
  }
}