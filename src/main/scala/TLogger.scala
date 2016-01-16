import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by shannon on 1/15/16.
  *
  *
  */

trait LogFunctionPrototypeT {

  //in trace implemtation there 3 vals, warning 3....
  type a = String => Unit
  type b = (String, Throwable) => Unit
  type c = (String, Array[AnyRef]) => Unit

  val logSimple:a
  val logWithException:b
  val logWithFormat:c

  type LogLevel = Int

  val level:LogLevel


//  val Warn: LogLevel = 2 //0x010
//  val Info: LogLevel = 4 //0x011
//  val Fatal: LogLevel = 8 //0x100


}

case class TraceInstance(logger:Logger) extends LogFunctionPrototypeT {

  override val level: LogLevel = 1 //0x001

  def traceA(message:String) = {
    if (logger.isTraceEnabled) {
      logger.trace(message)
    }
  }

  def traceB(message:String, throwable: Throwable) = {
    if (logger.isTraceEnabled) {
      logger.trace(message, throwable)
    }
  }

  def traceC(format:String, arguments:Array[AnyRef]) = {
    if (logger.isTraceEnabled) {
      logger.trace(format, arguments)
    }
  }

  override val logSimple:a = traceA
  override val logWithException:b = traceB
  override val logWithFormat:c = traceC

}

//acompany object
object TraceInstance {
    def apply(logger:Logger) = {
       new TraceInstance(logger)
    }
}



trait TLogger  {

       val logger:Logger = LoggerFactory.getLogger(super.getClass)

       def getName:String = logger.getName

      // val traceInstance = TraceInstance(logger)

       //TODO:add all other implementation
       def log(message:String)
              (implicit format:Option[String] = None, arguments:Option[Array[AnyRef]] = None, t:Option[Throwable] = None,
               instance:LogFunctionPrototypeT):Unit = {
          (t, format, arguments) match {

              //Trace
             case (None, None, None) =>
               instance.logSimple(message)

             case (t, None, None) =>
               instance.logWithException(message, t.get)

             case (None, format, arguments) =>
               instance.logWithFormat(format.get, arguments.get)



             case _ =>

           }
       }
}
