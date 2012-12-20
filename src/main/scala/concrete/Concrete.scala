package concrete

import java.net.URI
import java.net.URL
import java.util.Timer
import cspfj.ParameterManager
import cspfj.Solver
import cspfj.Statistic
import cspfj.Statistic
import cspfj.StatisticsManager
import cspfj.util.Waker
import cspom.compiler.ProblemCompiler
import cspfj.generator.ProblemGenerator
import cspom.CSPOM

object Concrete extends App {

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

  def options(o: Map[Symbol, Any], args: List[String]): Map[Symbol, Any] = args match {
    case "-D" :: opts :: tail => {
      val p = params(Nil, opts.split(":").toList)
      options(o + ('D -> p), tail)
    }
    case "-sql" :: option :: tail =>
      options(o + ('SQL -> option), tail)
    case "-control" :: tail => options(o + ('Control -> None), tail)
    case "-time" :: t :: tail => options(o + ('Time -> t.toInt), tail)
    case "-cl" :: tail => options(o + ('CL -> None), tail)
    case file :: Nil => o + ('file -> file)
    case unknown :: _ => throw new IllegalArgumentException(unknown)
    case _ => throw new IllegalArgumentException("Parameter expected")
  }

  val opt = try {
    options(Map.empty, args.toList)
  } catch {
    case e: IllegalArgumentException =>
      println(e.getMessage)
      println(help)
      sys.exit(1)
  }

  opt.get('D) match {
    case Some(p: List[(String, String)]) => for ((option, value) <- p) {
      ParameterManager.parse(option, value)
    }
    case _ =>
  }

  val statistics = new StatisticsManager()
  statistics.register("concrete", this)

  val file = new URL(opt('file).asInstanceOf[String])

  def load(file: URL) = {
    val (problem, lT) = StatisticsManager.time(CSPOM.load(file))

    val (_, cT) = StatisticsManager.time(ProblemCompiler.compile(problem))

    //println(problem)

    val (cProblem, gT) = StatisticsManager.time(ProblemGenerator.generate(problem))

    problem.closeRelations()

    (lT, cT, gT, Solver.factory(cProblem), problem)
  }

  var (lT, cT, gT, writer, solver, problem) =
    if (opt.contains('CL)) {

      val config = ParameterManager.toXML.toString
      val writer = opt.get('SQL) match {
        case None => new ConsoleWriter(config)
        case Some(url) => new SQLWriter(new URI(url.toString), file, config)
      }

      val (lT, cT, gT, solver, problem) = load(file)

      (lT, gT, cT, writer, solver, problem)
    } else {

      val (lT, cT, gT, solver, problem) = load(file)
      val config = ParameterManager.toXML.toString
      val writer = opt.get('SQL) match {
        case None => new ConsoleWriter(config)
        case Some(url) => new SQLWriter(new URI(url.toString), file, config)
      }

      (lT, gT, cT, writer, solver, problem)
    }

  @Statistic
  val loadTime = lT

  @Statistic
  val compileTime = cT

  @Statistic
  val generationTime = gT

  //var exc: Option[Throwable] = None

  //  val thread = new Thread() {
  //    override def run() {
  //      try {
  //        val solution = solver.nextSolution
  //        writer.solution(solution, problem)
  //        if (solution.isSat && opt.contains('Control)) {
  //          val failed = problem.controlInt(solution.get);
  //          println("Falsified constraints : " + failed.toString)
  //        }
  //
  //        writer.write(statistics)
  //        writer.write(solver.statistics)
  //      } catch {
  //        case e: Throwable => exc = e
  //      }
  //    }
  //  }

  val waker = new Timer()
  for (t <- opt.get('Time)) {
    waker.schedule(new Waker(Thread.currentThread()), t.asInstanceOf[Int] * 1000);
  }
  
  val sstats = solver.statistics

  try {
    val solution = solver.nextSolution
    writer.solution(solution, problem)
    if (solution.isSat && opt.contains('Control)) {
      val failed = problem.controlInt(solution.get);
      println("Falsified constraints : " + failed.toString)
    }
  } catch {
    case e: OutOfMemoryError => solver = null; problem = null; writer.error(e)
    case e: Throwable => writer.error(e)
  } finally {
    waker.cancel()
    writer.write(statistics)
    writer.write(sstats)
    writer.disconnect()
  }

}
