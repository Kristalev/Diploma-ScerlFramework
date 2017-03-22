package myframework

import com.ericsson.otp.erlang.OtpErlangObject

/**
  *Сообщение для [[ErlangAdapteeActor]].
  * @param msg сообщение для обработки [[ErlangAdapteeActor.erlangBehavior()]]
  */
case class ErlangMessage(msg:OtpErlangObject)

/**
  * Сообщение для [[ErlangAdapteeActor]].
  * @param msg сообщение для обработки [[ErlangAdapteeActor.scalaBehavior()]]
  */
case class ScalaMessage(msg:Any)
