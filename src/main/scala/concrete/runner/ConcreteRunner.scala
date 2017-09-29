package concrete.runner

import java.security.InvalidParameterException

import com.typesafe.scalalogging.LazyLogging
import concrete.runner.sql.SQLWriter
import concrete.{ParameterManager, Solver, Variable}
import cspom.compiler.CSPOMCompiler
import cspom.{Statistic, StatisticsManager, UNSATException}
import org.scalameter.Quantity

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

trait ConcreteRunner extends LazyLogging {

  //logger.addHandler(new MsLogHandler)
  val statistics = new StatisticsManager()
  @Statistic
  var loadTime: Quantity[Double] = _

  /**
    * Loads problem and builds solver according to given parameters and args
    *
    * @param pm
    * @param args
    * @return
    */
  def load(pm: ParameterManager, args: Seq[String]): Try[Solver]

  def description(args: Seq[String]): String

  def run(args: Array[String]): Result = {
    val (pm, remaining) = try {
      options(args)
    } catch {
      case e: IllegalArgumentException =>
        println(e.getMessage)
        println(help)
        sys.exit(1)
    }


    //    val optimize: String = pm.getOrElse("optimize", "sat")
    //
    //    val optimizeVar: Option[String] = pm.get[String]("optimizeVar")

    val writer: ConcreteWriter =
      if (pm.contains("sql")) {
        new SQLWriter(pm, description(remaining), statistics)
      } else {
        defaultWriter(pm, description(remaining), statistics)
      }

    Runtime.getRuntime.addShutdownHook(new Finisher(Thread.currentThread(), writer))

    statistics.register("runner", this)
    statistics.register("CSPOMCompiler", CSPOMCompiler)
    //statistics.register("problemGenerator", ProblemGenerator)

    // Regular try/catch required to catch OutOfMemoryErrors
    val status = try {
      val (tryLoad, lT) = StatisticsManager.measureTry[Solver, Unit, Double] {
        load(pm, remaining)
      }
      loadTime = lT

      val stat = for (solv <- tryLoad) yield {

        statistics.register("solver", solv)
        // solver = Some(solv)
        val r = if (pm.contains("a") || solv.optimises.nonEmpty) {
          for (s <- solv.toIterable) {
            solution(solv, s, writer, pm)
          }
          FullExplore
        } else {
          solv.toIterable.headOption match {
            case None => FullExplore
            case Some(s) =>
              solution(solv, s, writer, pm)
              Unfinished(None)
          }
        }
        for (s <- pm.unused) {
          logger.warn(s"Unused parameter : $s")
        }
        r
      }

      stat.get
    } catch {
      case e: UNSATException =>
        writer.error(e)
        FullExplore
      case e: Throwable =>
        writer.error(e)
        Error(e)
    }


    writer.end = status
    status
  }

  def help =
    """
    Usage : Concrete file
    """

  def options(args: Array[String]): (ParameterManager, Seq[String]) =
    args.foldLeft((new ParameterManager, new ArrayBuffer[String])) {
      case ((pm, realArgs), u) =>
        if (u.startsWith("-")) {
          val (key, value) = u.split("=") match {
            case Array(key, value) => (key.drop(1), value)
            case Array(tag) => (tag.drop(1), Unit)
            case e => throw new InvalidParameterException(e.mkString("[", ", ", "]"))
          }
          (pm.updated(key, value), realArgs)
        } else {
          (pm, realArgs += u)
        }
    }

  def defaultWriter(pm: ParameterManager, problem: String, sm: StatisticsManager): ConcreteWriter =
    new FZWriter(pm, problem, statistics)

  final def solution(solver: Solver, sol: Map[Variable, Any], writer: ConcreteWriter, pm:ParameterManager): Unit = {
    val objective = solver.optimises.map(sol)

    writer.solution(output(sol, objective), objective)
    if (pm.contains("control")) {
      for (failed <- control(sol, objective)) {
        throw new IllegalStateException("Control error: " + failed)
      }
    }
  }

  def output(solution: Map[Variable, Any], obj: Option[Any]): String = {
    solution.map {
      case (variable, value) => s"${variable.name} = $value"
    }.mkString("\n")

  }

  def control(solution: Map[Variable, Any], obj: Option[Any]): Option[String]

}

