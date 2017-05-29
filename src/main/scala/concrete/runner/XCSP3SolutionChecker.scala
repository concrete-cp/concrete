package concrete.runner

import java.io.{File, StringReader}
import java.net.URL
import java.nio.file.{Files, StandardCopyOption}

import com.typesafe.scalalogging.LazyLogging
import concrete.util.TryWith
import org.apache.commons.compress.compressors.CompressorStreamFactory
import org.postgresql.util.ReaderInputStream

import scala.collection.JavaConverters._
import scala.xml.Elem

class XCSP3SolutionChecker(file: URL) extends LazyLogging {

  val temp: File = File.createTempFile("xcsp", new File(file.getFile).getName + ".xml")
  temp.deleteOnExit()

  TryWith {
    val in = file.openStream
    new CompressorStreamFactory().createCompressorInputStream(in)
  } { in =>
    Files.copy(in, temp.toPath, StandardCopyOption.REPLACE_EXISTING)
  }
    .get

  //file.openStream()

  //Resource.fromURL(file).copyDataTo(tmpPath.outputStream())


  def checkSolution(solution: Elem): Option[String] = {
    val sc = new org.xcsp.checker.SolutionChecker(
      temp.getPath,
      new ReaderInputStream(new StringReader(s""""$solution"""")))

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
