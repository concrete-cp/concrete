package concrete.runner

import java.net.URI
import java.security.InvalidParameterException
import java.util.Timer
import com.typesafe.scalalogging.LazyLogging
import concrete.ParameterManager
import concrete.Problem
import concrete.Solver
import concrete.Variable
import concrete.generator.ProblemGenerator
import concrete.runner.sql.SQLWriter
import cspom.Statistic
import cspom.StatisticsManager
import cspom.TimedException
import cspom.compiler.CSPOMCompiler
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMVariable
import concrete.SolverFactory
import java.util.TimerTask
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import cspom.UNSATException
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

trait ConcreteRunner extends LazyLogging {

  def help = """
    Usage : Concrete file
    """

  //def isSwitch(s: String) = (s(0) == '-')
  'SQL
  'D
  'Control
  'Time

  //logger.addHandler(new MsLogHandler)

  private def params(o: List[(String, String)], options: List[String]): List[(String, String)] =
    if (options.isEmpty) {
      o
    } else {
      try {
        val Array(key, value) = options.head.split("=")
        params((key, value) :: o, options.tail)
      } catch {
        case e: MatchError => throw new InvalidParameterException(options.toString)
      }
    }

  def options(args: List[String], o: Map[Symbol, Any] = Map.empty, realArgs: List[String]): (Map[Symbol, Any], List[String]) = args match {
    case Nil => (o, realArgs.reverse)
    case "-P" :: opts :: tail => {
      val p = params(Nil, opts.split(":").toList)
      options(tail, o + ('D -> p), realArgs)
    }
    case "-sql" :: option :: tail =>
      options(tail, o + ('SQL -> option), realArgs)
    case "-control" :: tail             => options(tail, o + ('Control -> None), realArgs)
    case "-time" :: t :: tail           => options(tail, o + ('Time -> t.toInt), realArgs)
    case "-a" :: tail                   => options(tail, o + ('all -> Unit), realArgs)
    case "-s" :: tail                   => options(tail, o + ('stats -> Unit), realArgs)
    case u :: tail if u.startsWith("-") => options(tail, o + ('unknown -> u), realArgs)
    //    case "-cl" :: tail => options(tail, o + ('CL -> None))
    case u :: tail                      => options(tail, o, u :: realArgs)
  }

  def load(args: List[String], opt: Map[Symbol, Any]): Try[concrete.Problem]

  def applyParametersPre(opt: Map[Symbol, Any]): Unit = {}

  def applyParametersPost(s: Solver, opt: Map[Symbol, Any]): Unit = {}

  def description(args: List[String]): String

  @Statistic
  var loadTime: Double = _

  val pm = new ParameterManager
  val statistics = new StatisticsManager()

  def run(args: Array[String]): Try[Boolean] = {

    //var status: RunnerStatus = Unknown

    val (opt, remaining) = try {
      options(args.toList, realArgs = Nil)
    } catch {
      case e: IllegalArgumentException =>
        println(e.getMessage)
        println(help)
        sys.exit(1)
    }

    for (u <- opt.get('unknown)) {
      logger.warn("Unknown options: " + u)
    }

    opt.get('D).collect {
      case p: Seq[_] => for ((option, value) <- p.map { _.asInstanceOf[(String, String)] }) {
        pm(option) = value
      }
    }

    val optimize: String = pm.getOrElse("optimize", "sat")

    val optimizeVar: Option[String] = pm.get[String]("optimizeVar")

    val writer: ConcreteWriter =
      opt.get('SQL).map(url => new SQLWriter(new URI(url.toString), pm)).getOrElse {
        new ConsoleWriter(opt)
      }

    writer.problem(description(remaining))

    statistics.register("runner", this)
    statistics.register("CSPOMCompiler", CSPOMCompiler)
    //statistics.register("problemGenerator", ProblemGenerator)

    writer.parameters(pm.toXML)

    //val waker = new Timer()
    //try {

    val (tryLoad, lT) = StatisticsManager.timeTry {
      load(remaining, opt)
    }
    loadTime = lT

    val status = tryLoad
      .map { problem =>
        val f = Future {
          applyParametersPre(opt)
          val solver = new SolverFactory(pm)(problem)

          statistics.register("solver", solver)
          applyParametersPost(solver, opt)
          //println(solver.problem)

          //        for (time <- opt.get('Time)) {
          //          val thread = Thread.currentThread
          //          waker.schedule(
          //            new TimerTask {
          //              def run = thread.interrupt()
          //            },
          //            time.asInstanceOf[Int] * 1000);
          //        }

          optimize match {
            case "sat" =>
            case "min" =>
              solver.minimize(solver.problem.variable(optimizeVar.get))
            case "max" =>
              solver.maximize(solver.problem.variable(optimizeVar.get))
          }

          val result = solver.nonEmpty
          if (opt.contains('all)) {
            for (s <- solver) {
              solution(s, writer, opt)
            }
          } else if (solver.isOptimizer) {
            for (s <- solver.toIterable.lastOption) {
              solution(s, writer, opt)
            }
          } else {
            for (s <- solver.toIterable.headOption) {
              solution(s, writer, opt)
            }
          }
          result
        }
        concurrent.Await.result(f, opt.get('Time).map(_.asInstanceOf[Int].milliseconds).getOrElse(Duration.Inf))

      }
      .recoverWith {
        case e: UNSATException =>
          writer.error(e)
          Success(false)
        case e: Throwable =>
          writer.error(e)
          Failure(e)
      }

    //waker.cancel()
    writer.write(statistics)
    for (s <- pm.unused) {
      logger.warn(s"Unused parameter : $s")
    }
    writer.disconnect(status)
    status
  }

  final def solution(sol: Map[Variable, Any], writer: ConcreteWriter, opt: Map[Symbol, Any]): Unit = {
    writer.solution(output(sol))
    if (opt.contains('Control)) {
      for (failed <- control(sol)) {
        throw new IllegalStateException("Falsified constraints : " + failed)
      }
    }
  }

  def output(solution: Map[Variable, Any]): String = {
    solution.map {
      case (variable, value) => s"${variable.name} = $value"
    }.mkString("\n")

  }

  def control(solution: Map[Variable, Any]): Option[String]

}

