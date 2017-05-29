package concrete.runner

import java.security.InvalidParameterException
import java.util.Timer

import com.typesafe.scalalogging.LazyLogging
import concrete.runner.sql.SQLWriter
import concrete.{ParameterManager, Problem, Solver, Variable}
import cspom.compiler.CSPOMCompiler
import cspom.{Statistic, StatisticsManager, UNSATException}
import org.scalameter.Quantity

import scala.util.Try

trait ConcreteRunner extends LazyLogging {

  val pm = new ParameterManager

  //def isSwitch(s: String) = (s(0) == '-')
  'SQL
  'D
  'Control
  'Time
  'iteration

  //logger.addHandler(new MsLogHandler)
  val statistics = new StatisticsManager()
  @Statistic
  var loadTime: Quantity[Double] = _
  @Statistic
  var benchTime: Quantity[Double] = _

  def help =
    """
    Usage : Concrete file
    """

  def options(args: List[String], o: Map[Symbol, Any] = Map.empty, realArgs: List[String]): (Map[Symbol, Any], List[String]) = args match {
    case Nil => (o, realArgs.reverse)
    case "-P" :: opts :: tail => {
      val p = opts.split(":")
        .map(_.split("="))
        .map {
          case Array(key, value) => (key, value)
          case Array(tag) => (tag, Unit)
          case e => throw new InvalidParameterException(e.toString)
        }
        .toSeq

      options(tail, o + ('D -> p), realArgs)
    }
    case "-sql" :: tail =>
      options(tail, o + ('SQL -> Unit), realArgs)
    case "-control" :: tail => options(tail, o + ('Control -> Unit), realArgs)
    case "-time" :: t :: tail => options(tail, o + ('Time -> t.toInt), realArgs)
    case "-a" :: tail => options(tail, o + ('all -> Unit), realArgs)
    case "-s" :: tail => options(tail, o + ('stats -> Unit), realArgs)
    case "-it" :: it :: tail => options(tail, o + ('iteration -> it.toInt), realArgs)
    case u :: tail if u.startsWith("-") => options(tail, o + ('unknown -> u), realArgs)
    //    case "-cl" :: tail => options(tail, o + ('CL -> None))
    case u :: tail => options(tail, o, u :: realArgs)
  }

  def load(args: List[String], opt: Map[Symbol, Any]): Try[concrete.Problem]

  def applyParametersPre(p: Problem, opt: Map[Symbol, Any]): Unit = {}

  def applyParametersPost(s: Solver, opt: Map[Symbol, Any]): Unit = {}

  def description(args: List[String]): String

  def defaultWriter(opt: Map[Symbol, Any], sm: StatisticsManager): ConcreteWriter = new FZWriter(opt, statistics)

  def run(args: Array[String]): Result = {

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
        for ((option, value) <- p.map {
          _.asInstanceOf[(String, Any)]
        }) {
          pm(option) = value
        }
    }

    val optimize: String = pm.getOrElse("optimize", "sat")

    val optimizeVar: Option[String] = pm.get[String]("optimizeVar")

    val writer: ConcreteWriter =
      if (opt.contains('SQL)) {
        new SQLWriter(pm, statistics)
      } else {
        defaultWriter(opt, statistics)
      }

    Runtime.getRuntime().addShutdownHook(new Finisher(Thread.currentThread(), writer))

    val timer = opt.get('Time)
      .map { case time: Int =>
        val t = new Timer()
        t.schedule(new Killer(Runtime.getRuntime), time * 1000)
        t
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

    val (tryLoad, lT) = StatisticsManager.measureTry[Problem, Unit, Double] {
      load(remaining, opt)
    }
    loadTime = lT

    val r = tryLoad.map { problem =>

      applyParametersPre(problem, opt)

      val solver = Solver(problem, pm)

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
        if (opt.contains('all) || solv.optimises.nonEmpty) {
          for (s <- solv.toIterable) {
            solution(solv, s, writer, opt)
          }
          FullExplore
        } else {
          solv.toIterable.headOption match {
            case None => FullExplore
            case Some(s) =>
              solution(solv, s, writer, opt)
              Unfinished(None)
          }
        }
      }


    //    val timeout = opt.get('Time).map(_.asInstanceOf[Int].seconds).getOrElse(Duration.Inf)
    //
    //    val r = Try(concurrent.Await.result(f, timeout))
    //      .recoverWith {
    //        case e: TimeoutException =>
    //          logger.info("Cancelling")
    //          f.cancel()
    //          var tries = 100
    //          while (tries > 0 && solver.exists(_.running)) {
    //            logger.info("Waiting for interruption")
    //            Thread.sleep(100)
    //            tries -= 1
    //          }
    //          if (tries >= 0) {
    //            Failure(e)
    //          } else {
    //            Failure(new IllegalStateException("Could not interrupt search process", e))
    //          }
    //
    //      }

    val status: Result = r.recover {
      case e: UNSATException =>
        writer.error(e)
        FullExplore
      case e: Throwable =>
        writer.error(e)
        Error(e)
    }
      .get

    for (s <- pm.unused) {
      logger.warn(s"Unused parameter : $s")
    }
    for (t: Timer <- timer) {
      t.cancel()
    }
    writer.end = status
    status
  }

  final def solution(solver: Solver, sol: Map[Variable, Any], writer: ConcreteWriter, opt: Map[Symbol, Any]): Unit = {
    val objective = solver.optimises.map(sol)

    writer.solution(output(sol, objective), objective)
    if (opt.contains('Control)) {
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

