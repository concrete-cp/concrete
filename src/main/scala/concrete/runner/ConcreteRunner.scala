package concrete.runner

import java.net.URI
import java.security.InvalidParameterException
import java.util.Timer

import concrete.ParameterManager
import concrete.Problem
import concrete.Solver
import concrete.generator.FailedGenerationException
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.ConcretePatterns
import concrete.util.Waker
import cspom.CSPOM
import com.typesafe.scalalogging.slf4j.LazyLogging
import cspom.Statistic
import cspom.StatisticsManager
import cspom.TimedException
import cspom.compiler.ProblemCompiler

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
    case "-D" :: opts :: tail => {
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

  def load(args: List[String]): Problem = {
    val cspom = loadCSPOM(args)
    ProblemCompiler.compile(cspom, ConcretePatterns())
    _cProblem = Some(cspom)
    ProblemGenerator.generate(cspom)
  }

  def loadCSPOM(args: List[String]): CSPOM = ???

  def description(args: List[String]): String

  @Statistic
  var loadTime: Double = _

  def run(args: Array[String]) {
    val (opt, remaining) = try {
      options(args.toList)
    } catch {
      case e: IllegalArgumentException =>
        println(e.getMessage)
        println(help)
        sys.exit(1)
    }

    val writer: ConcreteWriter =
      //opt.get('SQL).map(url => new SQLWriter(new URI(url.toString))).getOrElse 
      {
        new ConsoleWriter()
      }

    writer.problem(description(remaining))

    opt.get('D) match {
      case Some(p: List[(String, String)]) => for ((option, value) <- p) {
        ParameterManager.parse(option, value)
      }
      case _ =>
    }

    val statistics = new StatisticsManager()
    statistics.register("concrete", this)

    var sstats: Option[StatisticsManager] = None

    val waker = new Timer()
    try {

      val solver = try {
        val (solver, lT) = StatisticsManager.time {
          val problem = load(remaining)
          Solver(problem)
        }
        loadTime = lT
        solver
      } catch {
        case e: TimedException =>
          loadTime = e.time
          throw e.getCause()
      }

      //println(solver.problem)

      writer.parameters(ParameterManager.toXML)

      for (t <- opt.get('Time)) {
        waker.schedule(new Waker(Thread.currentThread()), t.asInstanceOf[Int] * 1000);
      }

      sstats = Some(solver.statistics)

      val solution = solver.toIterable.headOption
      writer.solution(solution, this)
      if (solution.isDefined && opt.contains('Control)) {
        val failed = control(solution.get);
        if (failed.isDefined) throw new IllegalStateException("Falsified constraints : " + failed.get)
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

  def output(solution: Map[String, Any]): String = {
    cProblem.namedExpressions.collect {
      case (name, variable) if (!variable.hasParam("var_is_introduced")) => s"$name = ${solution(name)}"
    }.mkString("\n")
  }

  def control(solution: Map[String, Any]): Option[String]

}

