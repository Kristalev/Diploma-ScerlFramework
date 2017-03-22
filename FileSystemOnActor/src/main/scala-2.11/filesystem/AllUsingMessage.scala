package filesystem

import com.ericsson.otp.erlang.{OtpErlangObject, OtpErlangPid}

/**
  * Created by Данил on 06.06.2016.
  */


/**Собщение об успешно подключенном узле системы
  *
  * @param nodeName имя узла
  * @param mapProc карта процессов подключенного узла
  */
case class ConnectedNode(nodeName:String,mapProc:Map[String,OtpErlangPid])

/**Сообщение о неподключенном узле
  *
  * @param nodeName имя узла
  */
case class NodeNotConnect(nodeName:String)

/**
  * Сообщение для начала инициализации системы.
  * @param nodesList узлы для инициализации
  */
case class InitCommand(nodesList:Set[String])

/**
  * Cообщение об успешной инициализации системы
  * @param nodes успешно подключеные узлы
  */
case class InitDone(nodes:Set[ConnectedNode])

/**
  * Сообщение об неполадках во время инициализации
  */
case object InitError

/**
  * Абстрактный класс для команд к файлам
  */
abstract class CommandToFile

/**
  * Команда для удаления файла
  * @param file имя удаляемого файла
  */
case class DeleteFile(file: String) extends CommandToFile

/**
  *Команда для переименования файла
  * @param file имя файла
  * @param newFileName новое имя файла
  */
case class RenameFile(file: String, newFileName: String) extends CommandToFile

/**
  * Команда для копирования файла
  *
  * @param file имя файла
  * @param newPlace имя копии файла file
  */
case class CopyFile(file: String, newPlace: String) extends CommandToFile

/**
  * Cообщение с командой для конкретного узла
  * @param node имя узла, к которому адресована команда
  * @param cmmand команда для файла на узле
  */
case class CommandToNode(node: String, cmmand: CommandToFile)

/**
  * Cообщение с командами для файловой системы
  * @param commands команды для файловой системы
  */
case class CommandToFS(commands: Set[CommandToNode])

/**
  * Cообщение об удалении файла
  * @param node имя узла, на котором был удален файл
  * @param file имя удаленного файла
  */
case class FileDeleted(node: String, file: String)

/**
  * Cообщение о переименовании файла
  * @param node имя узла, на котором был переименован файл
  * @param oldFile старое имя файла
  * @param newFile новое имя файла
  */
case class FileRenamed(node:String, oldFile:String, newFile:String)

/**
  * Cообщение о копировании файла
  * @param node имя узла, на котором был скопирован файл
  * @param originalFile имя оригинала файла
  * @param copyFile имя скопированного файла
  * @param pidCopy идентификатор процесса обслуживающего новый файл
  */
case class FileCopied(node:String, originalFile:String, copyFile:String, pidCopy:OtpErlangPid)
