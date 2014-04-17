package concrete.runner

import java.net.URI
import java.security.InvalidParameterException
import java.util.Timer
import com.typesafe.scalalogging.slf4j.LazyLogging
import concrete.ParameterManager
import concrete.Problem
import concrete.Solver
import concrete.Variable
import concrete.generator.ProblemGenerator
import concrete.runner.sql.SQLWriter
import concrete.util.Waker
import cspom.Statistic
import cspom.StatisticsManager
import cspom.TimedException
import cspom.compiler.ProblemCompiler
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMVariable
import concrete.SolverFactory

trait ConcreteRunner {

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

  def options(args: List[String], o: Map[Symbol, Any] = Map.empty, unknown: List[String] = Nil): (Map[Symbol, Any], List[String]) = args match {
    case Nil => (o, unknown.reverse)
    case "-P" :: opts :: tail => {
      val p = params(Nil, opts.split(":").toList)
      options(tail, o + ('D -> p), unknown)
    }
    case "-sql" :: option :: tail =>
      options(tail, o + ('SQL -> option), unknown)
    case "-control" :: tail => options(tail, o + ('Control -> None))
    case "-time" :: t :: tail => options(tail, o + ('Time -> t.toInt))
    //    case "-cl" :: tail => options(tail, o + ('CL -> None))
    case u :: tail => options(tail, o, u :: unknown)
  }

  def load(args: List[String]): concrete.Problem

  def applyParameters(s: Solver): Unit = {}

  def description(args: List[String]): String

  @Statistic
  var loadTime: Double = _

  val pm = new ParameterManager
  val statistics = new StatisticsManager()

  def run(args: Array[String]) {
    val (opt, remaining) = try {
      options(args.toList)
    } catch {
      case e: IllegalArgumentException =>
        println(e.getMessage)
        println(help)
        sys.exit(1)
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
        new ConsoleWriter()
      }

    writer.problem(description(remaining))

    statistics.register("runner", this)
    statistics.register("problemCompiler", ProblemCompiler)
    //statistics.register("problemGenerator", ProblemGenerator)

    val waker = new Timer()
    try {

      val problem = try {
        val (problem, lT) = StatisticsManager.time {
          load(remaining)
        }
        loadTime = lT
        problem
      } catch {
        case e: TimedException =>
          loadTime = e.time
          // Required to obtain a configId
          writer.parameters(pm.toXML)
          throw e.getCause()
      }

      val solver = new SolverFactory(pm)(problem)
      // Loading the solver will initialize many parameters
      writer.parameters(pm.toXML)

      statistics.register("solver", solver)
      applyParameters(solver)
      //println(solver.problem)

      for (t <- opt.get('Time)) {
        waker.schedule(new Waker(Thread.currentThread()), t.asInstanceOf[Int] * 1000);
      }

      optimize match {
        case "sat" =>
        case "min" =>
          solver.minimize(solver.problem.variable(optimizeVar.get))
        case "max" =>
          solver.maximize(solver.problem.variable(optimizeVar.get))
      }

      if (solver.isOptimizer) {
        if (!solver.hasNext) {
          solution(None, writer, opt)
        } else for (s <- solver) {
          solution(Some(s), writer, opt)
        }
      } else {
        solution(solver.toIterable.headOption, writer, opt)
      }

    } catch {
      case e: Throwable =>
        writer.error(e)

    } finally {
      waker.cancel()
      writer.write(statistics)
      writer.disconnect()
    }
  }

  final def solution(sol: Option[Map[Variable, Any]], writer: ConcreteWriter, opt: Map[Symbol, Any]): Unit = {
    writer.solution(output(sol))
    if (opt.contains('Control)) {
      for (s <- sol; failed <- control(s)) {
        throw new IllegalStateException("Falsified constraints : " + failed)
      }
    }
  }

  def output(solution: Option[Map[Variable, Any]]): String = {
    solution match {
      case None => "UNSAT"
      case Some(s) => s.map {
        case (variable, value) => s"${variable.name} = $value"
      }.mkString("\n")
    }

  }

  def control(solution: Map[Variable, Any]): Option[String]

}

