package concrete.runner

import java.security.InvalidParameterException
import com.typesafe.scalalogging.LazyLogging
import concrete.ParameterManager
import concrete.Solver
import concrete.Variable
import concrete.runner.sql.SQLWriter
import cspom.Statistic
import cspom.StatisticsManager
import cspom.compiler.CSPOMCompiler
import concrete.SolverFactory
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import cspom.UNSATException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import org.scalameter.Quantity
import concrete.util.KlangCancellableFuture
import java.util.concurrent.TimeoutException
import concrete.Problem

trait ConcreteRunner extends LazyLogging {

  def help = """
    Usage : Concrete file
    """

  //def isSwitch(s: String) = (s(0) == '-')
  'SQL
  'D
  'Control
  'Time
  'iteration

  //logger.addHandler(new MsLogHandler)

  def options(args: List[String], o: Map[Symbol, Any] = Map.empty, realArgs: List[String]): (Map[Symbol, Any], List[String]) = args match {
    case Nil => (o, realArgs.reverse)
    case "-P" :: opts :: tail => {
      val p = opts.split(":")
        .map(_.split("="))
        .map {
          case Array(key, value) => (key, value)
          case Array(tag)        => (tag, Unit)
          case e                 => throw new InvalidParameterException(e.toString)
        }
        .toSeq

      options(tail, o + ('D -> p), realArgs)
    }
    case "-sql" :: tail =>
      options(tail, o + ('SQL -> Unit), realArgs)
    case "-control" :: tail             => options(tail, o + ('Control -> Unit), realArgs)
    case "-time" :: t :: tail           => options(tail, o + ('Time -> t.toInt), realArgs)
    case "-a" :: tail                   => options(tail, o + ('all -> Unit), realArgs)
    case "-s" :: tail                   => options(tail, o + ('stats -> Unit), realArgs)
    case "-it" :: it :: tail            => options(tail, o + ('iteration -> it.toInt), realArgs)
    case u :: tail if u.startsWith("-") => options(tail, o + ('unknown -> u), realArgs)
    //    case "-cl" :: tail => options(tail, o + ('CL -> None))
    case u :: tail                      => options(tail, o, u :: realArgs)
  }

  def load(args: List[String], opt: Map[Symbol, Any]): Try[concrete.Problem]

  def applyParametersPre(p: Problem, opt: Map[Symbol, Any]): Unit = {}

  def applyParametersPost(s: Solver, opt: Map[Symbol, Any]): Unit = {}

  def description(args: List[String]): String

  @Statistic
  var loadTime: Quantity[Double] = _

  val pm = new ParameterManager
  val statistics = new StatisticsManager()

  @Statistic
  var benchTime: Quantity[Double] = _

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

    logger.info((opt, remaining).toString)

    for (u <- opt.get('unknown)) {
      logger.warn("Unknown options: " + u)
    }

    opt.get('D).map {
      case p: Seq[_] =>
        for ((option, value) <- p.map { _.asInstanceOf[(String, Any)] }) {
          pm(option) = value
        }
    }

    val optimize: String = pm.getOrElse("optimize", "sat")

    val optimizeVar: Option[String] = pm.get[String]("optimizeVar")

    val writer: ConcreteWriter =
      if (opt.contains('SQL)) {
        new SQLWriter(pm, statistics)
      } else {
        new ConsoleWriter(opt, statistics)
      }

    for (it <- opt.get('iteration)) {
      pm("iteration") = it
    }

    writer.problem(description(remaining))

    statistics.register("runner", this)
    statistics.register("CSPOMCompiler", CSPOMCompiler)
    //statistics.register("problemGenerator", ProblemGenerator)

    writer.parameters(pm)

    //val waker = new Timer()
    //try {

    var solver: Option[Solver] = None

    val f = KlangCancellableFuture {
      try {
        val (tryLoad, lT) = StatisticsManager.measureTry {
          load(remaining, opt)
        }
        loadTime = lT

        tryLoad.map { problem =>

          applyParametersPre(problem, opt)

          val solver = new SolverFactory(pm)(problem)

          statistics.register("solver", solver)
          applyParametersPost(solver, opt)

          optimize match {
            case "sat" =>
            case "min" =>
              solver.minimize(solver.problem.variable(optimizeVar.get))
            case "max" =>
              solver.maximize(solver.problem.variable(optimizeVar.get))
          }

          solver
        }
          .map { solv =>
            solver = Some(solv)
            val result = solv.nonEmpty
            if (opt.contains('all)) {
              for (s <- solv) {
                solution(s, writer, opt)
              }
            } else if (solv.optimises.nonEmpty) {
              for (s <- solv.toIterable.lastOption) {
                solution(s, writer, opt)
              }
            } else {
              for (s <- solv.toIterable.headOption) {
                solution(s, writer, opt)
              }
            }
            result
          }
      } catch {
        // Avoids hanging in case of fatal error
        case any: Throwable => Failure(any)
      }
    }

    val timeout = opt.get('Time).map(_.asInstanceOf[Int].seconds).getOrElse(Duration.Inf)

    val r = Try(concurrent.Await.result(f, timeout))
      .recoverWith {
        case e: TimeoutException =>
          logger.info("Cancelling")
          f.cancel()
          var tries = 100
          while (tries > 0 && solver.exists(_.running)) {
            logger.info("Waiting for interruption")
            Thread.sleep(100)
            tries -= 1
          }
          if (tries >= 0) {
            Failure(e)
          } else {
            Failure(new IllegalStateException("Could not interrupt search process", e))
          }

      }

    val status = r.flatten
      .recoverWith {
        case e: UNSATException =>
          writer.error(e)
          Success(false)
        case e: Throwable =>
          writer.error(e)
          Failure(e)
      }

    status.foreach { _ =>
      for (s <- pm.unused) {
        logger.warn(s"Unused parameter : $s")
      }
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

