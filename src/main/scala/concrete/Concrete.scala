package concrete

import java.net.URL
import cspfj.ParameterManager
import cspfj.Statistic
import cspfj.StatisticsManager
import cspom.compiler.ProblemCompiler
import cspom.CSPOM
import cspfj.Solver
import cspfj.generator.ProblemGenerator
import java.net.URI
import cspfj.Problem
import java.sql.DriverManager
import java.util.Timer
import cspfj.util.Waker

object Concrete extends App {

  def help = """
    Usage : Concrete file
    """

  //def isSwitch(s: String) = (s(0) == '-')
  'SQL
  'D
  'Control
  'Time

  def params(o: List[(String, String)], options: List[String]): List[(String, String)] =
    if (options == Nil) o
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

  val (problem, lT) = StatisticsManager.time(CSPOM.load(file))

  @Statistic
  val loadTime = lT

  val (_, cT) = StatisticsManager.time(ProblemCompiler.compile(problem))

  @Statistic
  val compileTime = cT

  //println(problem)

  val (cProblem, gT) = StatisticsManager.time(ProblemGenerator.generate(problem))

  @Statistic
  val generationTime = gT

  var writer: ConcreteWriter = null
  var exc: Throwable = null

  {
    val solver = Solver.factory(cProblem)
    writer = opt.get('SQL) match {
      case None => new ConsoleWriter(ParameterManager.toXML.toString)
      case Some(url) => new SQLWriter(new URI(url.toString), file, ParameterManager.toXML.toString)
    }

    val thread = new Thread() {
      override def run() {
        try {
          val solution = solver.nextSolution
          writer.solution(solution, problem)
          if (solution.isSat && opt.contains('Control)) {
            val failed = problem.controlInt(solution.get);
            println("Falsified constraints : " + failed.toString)
          }
          //        } catch {
          //          case e: InterruptedException => throw e
          //        }
          writer.write(statistics)
          writer.write(solver.statistics)
        } catch {
          case e: Throwable => exc = e
        }
      }
    }

    val waker = new Timer()
    opt.get('Time) match {
      case Some(t: Int) => {
        //println("Setting time limit to " + t + " seconds")
        waker.schedule(new Waker(thread), t * 1000);
      }
      case _ =>
    }
    //
    thread.start()
    //t.wait()
    thread.join()
    waker.cancel()
  }
  if (exc ne null) writer.error(exc)

  writer.disconnect()

}