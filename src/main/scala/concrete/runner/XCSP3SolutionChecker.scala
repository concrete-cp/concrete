package concrete.runner

import java.io.{File, StringReader}
import java.net.URL
import java.nio.file.{Files, StandardCopyOption}

import com.typesafe.scalalogging.LazyLogging
import concrete.util.TryWith
import org.apache.commons.compress.compressors.CompressorStreamFactory
import org.postgresql.util.ReaderInputStream
import org.xcsp.checker.SolutionChecker

import scala.jdk.CollectionConverters._
import scala.util.Try


class XCSP3SolutionChecker(file: URL) extends LazyLogging {

  val temp: File = File.createTempFile("xcsp", new File(file.getFile).getName + ".xml")
  temp.deleteOnExit()

  TryWith {
    val in = file.openStream
    Try(new CompressorStreamFactory().createCompressorInputStream(in))
      .getOrElse(in)
  } { in =>
    Files.copy(in, temp.toPath, StandardCopyOption.REPLACE_EXISTING)
  }
    .get

  //file.openStream()

  //Resource.fromURL(file).copyDataTo(tmpPath.outputStream())

  def checkSolution(sol: Map[String, Any], obj: Option[Any], variables: Seq[String]): Option[String] = {
    val solution = XCSP3Concrete.xmlSolution(variables, sol, obj)
    val io = s"""s SATISFIABLE\n""" ++ solution.toString.split("\n").map("v " + _).mkString("\n")
    logger.info(io)
    val sc = new SolutionChecker(
      true,
      temp.getPath,
      new ReaderInputStream(new StringReader(io))) {
      override def endInstance(): Unit = ()
    }

    if (sc.violatedCtrs.isEmpty && sc.invalidObjs.isEmpty) {
      None
    } else {
      logger.error(s"INVALID Solution! (${sc.violatedCtrs.size + sc.invalidObjs.size}) errors)")
      Some(
        sc.violatedCtrs.asScala.map(c => "Violated constraint " + c).mkString("\n") +
          sc.invalidObjs.asScala.map(o => "Invalid Objective " + o).mkString("\n")
      )
    }
  }

}
