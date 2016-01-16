import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by shannon on 1/15/16.
  *
  *
  */

case class TraceInstance(logger:Logger)  {

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
}

object TraceInstance {
    def apply(logger:Logger) = {
       new TraceInstance(logger)
    }
}

trait TLogger {

       val logger:Logger = LoggerFactory.getLogger(super.getClass)

       def getName:String = logger.getName

       type LogLevel = Int
       type LogExecute = Option[String] => Unit
       type LogWithFormat = (Option[String], Option[Array[AnyRef]]) => Unit
       type LogWithThrowble = Option[Throwable] => Unit

       val Trace: LogLevel = 1 //0x001
       val Warn: LogLevel = 2 //0x010
       val Info: LogLevel = 4 //0x011
       val Fatal: LogLevel = 8 //0x100

       val traceInstance = TraceInstance(logger)

       //TODO:add all other implementation
       def log(message:String)
              (implicit format:Option[String] = None, t:Option[Throwable] = None, level:LogLevel = Info):Unit = {
          (level, t, format) match {

              //Trace
             case (Trace, None, None) =>
               traceInstance.trace(Some(message))

             case (Trace, t, None) =>

             case (Trace, None, format) =>


             //Warning
             case (Warn, None, None) =>

             case (Warn, t, None) =>

             case (Warn, None, format) =>

             //Info

             case _ =>

           }
       }
}
