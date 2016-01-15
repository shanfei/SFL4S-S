import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by shannon on 1/15/16.
  *
  *
  */


trait TTraceImpl extends TLogger {

  def trace(message:Option[String]) = {
    if (logger.isTraceEnabled) {
      logger.trace(message.get)
    }
  }

  def trace(message:String, throwable: Throwable) = {
    if (logger.isTraceEnabled) {
      logger.trace(message, throwable)
    }
  }

  def trace(format:String, arguments:Array[AnyRef]) = {
    if (logger.isTraceEnabled) {
      logger.trace(format, arguments)
    }
  }

  val traceLogExecute:logExecute = trace
  //TODO: add all other impl

}

trait TLogger {

       val logger:Logger = LoggerFactory.getLogger(super.getClass)

       def getName:String = logger.getName

       type LogLevel = Int
       type logExecute = Option[String] => Unit
       type LogMessage = Option[Array[AnyRef]] => logExecute
       type LogMessageWithThrowble = Option[Throwable] => logExecute

       val Trace: LogLevel = 1 //0x001
       val Warn: LogLevel = 2 //0x010
       val Info: LogLevel = 4 //0x011
       val Fatal: LogLevel = 8 //0x100

       val logExecuteImpl:logExecute


       //TODO:add all other implementation
       def log(t:Option[Throwable], level:LogLevel)(implicit format:Option[String] = None):logExecute = {
         (level, t, format) match {
             case (Trace, None, None) =>
               logExecuteImpl
           }
       }
}
