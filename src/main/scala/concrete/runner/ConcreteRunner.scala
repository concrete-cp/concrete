package concrete.runner

import java.net.URI
import java.security.InvalidParameterException
import java.util.Timer

import com.typesafe.scalalogging.slf4j.LazyLogging

import concrete.Parameter
import concrete.ParameterManager
import concrete.Problem
import concrete.Solver
import concrete.Variable
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.ConcretePatterns
import concrete.util.Waker
import concrete.runner.sql.SQLWriter
import cspom.CSPOM
import cspom.Statistic
import cspom.StatisticsManager
import cspom.TimedException
import cspom.compiler.ProblemCompiler
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMVariable

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

  var _cProblem: Option[CSPOM] = None

  def cProblem = _cProblem.get

  def load(args: List[String]): (Problem, Map[CSPOMVariable[_], Variable]) = {
    val cspom = loadCSPOM(args)
    ProblemCompiler.compile(cspom, ConcretePatterns())
    _cProblem = Some(cspom)
    ProblemGenerator.generate(cspom)
  }

  def loadCSPOM(args: List[String]): CSPOM = ???

  def applyParameters(s: Solver, variables: Map[CSPOMVariable[_], Variable]): Unit = {}

  def description(args: List[String]): String

  @Statistic
  var loadTime: Double = _

  @Parameter("optimize")
  var optimize: String = "sat"

  @Parameter("optimizeVar")
  var optimizeVar: String = ""

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
      case p: Seq[(String, String)] => for ((option, value) <- p) {
        ParameterManager.parse(option, value)
      }
    }

    val writer: ConcreteWriter =
      opt.get('SQL).map(url => new SQLWriter(new URI(url.toString))).getOrElse {
        new ConsoleWriter()
      }

    writer.problem(description(remaining))

    val statistics = new StatisticsManager()
    statistics.register("concrete", this)

    var sstats: Option[StatisticsManager] = None

    val waker = new Timer()
    try {

      val (solver, variables) = try {
        val (solver, lT) = StatisticsManager.time {
          val (problem, variables) = load(remaining)
          (Solver(problem), variables)
        }
        loadTime = lT
        solver
      } catch {
        case e: TimedException =>
          loadTime = e.time
          throw e.getCause()
      }

      applyParameters(solver, variables)
      //println(solver.problem)

      writer.parameters(ParameterManager.toXML)

      for (t <- opt.get('Time)) {
        waker.schedule(new Waker(Thread.currentThread()), t.asInstanceOf[Int] * 1000);
      }

      sstats = Some(solver.statistics)

      optimize match {
        case "sat" => solution(solver.toIterable.headOption, writer, opt)
        case "min" =>
          solver.minimize(solver.problem.variable(optimizeVar))
        case "max" =>
          solver.maximize(solver.problem.variable(optimizeVar))
      }

      if (solver.isOptimizer) {
        if (solver.isEmpty) {
          solution(None, writer, opt)
        } else for (s <- solver) {
          solution(Some(s), writer, opt)
        }
      } else {
        solution(solver.toIterable.headOption, writer, opt)
      }

      writer.write(statistics)
      writer.write(sstats.get)
    } catch {
      case e: Exception =>
        writer.error(e)
        writer.write(statistics)
        sstats.foreach(writer.write)

      case e: Throwable =>
        sstats = None
        writer.error(e)

    } finally {
      waker.cancel()
      writer.disconnect()
    }
  }

  def solution(sol: Option[Map[String, Any]], writer: ConcreteWriter, opt: Map[Symbol, Any]) {
    writer.solution(sol, this)
    for (
      s <- sol if opt.contains('Control);
      failed <- control(s)
    ) {
      throw new IllegalStateException("Falsified constraints : " + failed)
    }
  }

  def output(solution: Map[String, Any]): String = {
    cProblem.namedExpressions.collect {
      case (name, variable) if (!variable.hasParam("var_is_introduced")) => s"$name = ${solution(name)}"
    }.mkString("\n")
  }

  def control(solution: Map[String, Any]): Option[String]

}

