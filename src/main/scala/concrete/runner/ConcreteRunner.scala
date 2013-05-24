package concrete.runner

import java.net.URI
import java.util.Timer
import concrete.ParameterManager
import concrete.Solver
import concrete.Statistic
import concrete.Statistic
import concrete.StatisticsManager
import concrete.util.Waker
import cspom.compiler.ProblemCompiler
import concrete.generator.ProblemGenerator
import cspom.CSPOM
import concrete.Problem
import concrete.Parameter

trait ConcreteRunner {

  def help = """
    Usage : Concrete file
    """

  //def isSwitch(s: String) = (s(0) == '-')
  'SQL
  'D
  'Control
  'Time
  'CL

  def params(o: List[(String, String)], options: List[String]): List[(String, String)] =
    if (options == Nil) { o }
    else {
      val Array(key, value) = options.head.split("=")
      params((key, value) :: o, options.tail)
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
    case "-cl" :: tail => options(tail, o + ('CL -> None))
    case u :: tail => options(tail, o, u :: unknown)
  }

  var _cProblem: Option[CSPOM] = None

  def cProblem = _cProblem.get

  def load(args: List[String]): Problem = {
    val cspom = loadCSPOM(args)
    if (Concrete.compile) {
      ProblemCompiler.compile(cspom)
    }
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

    val writer: ConcreteWriter = opt.get('SQL).map(url => new SQLWriter(new URI(url.toString))).
      getOrElse(new ConsoleWriter())

    writer.problem(description(remaining))

    opt.get('D) match {
      case Some(p: List[(String, String)]) => for ((option, value) <- p) {
        ParameterManager.parse(option, value)
      }
      case _ =>
    }

    writer.parameters(ParameterManager.toXML.toString)

    val statistics = new StatisticsManager()
    statistics.register("concrete", this)

    var sstats: Option[StatisticsManager] = None

    val waker = new Timer()
    try {

      val (problem, lT) = StatisticsManager.time(load(remaining))
      val solver = Solver(problem)

      loadTime = lT

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

  def output(solution: Map[String, Int]): String
  def control(solution: Map[String, Int]): Option[String]

}

object Concrete {
  @Parameter("concrete.compile")
  val compile = true
}
