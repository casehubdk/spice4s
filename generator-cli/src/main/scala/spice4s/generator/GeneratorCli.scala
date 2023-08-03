package spice4s.generator

import fs2.io.file._
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import cats.effect._

object GeneratorCli
    extends CommandIOApp(
      name = "spice4s-client-codegen",
      header = "Generator cli for spicedb schemas"
    ) {

  val schemaOpt = Opts.option[String]("schema", help = "Path to the schema file", short = "s")
  val outOpt = Opts.option[String]("out", help = "Path to the output file", short = "o")

  override def main: Opts[IO[ExitCode]] =
    (schemaOpt, outOpt).mapN { case (schema, out) =>
      val sp = Path(schema)
      val op = Path(out)

      Generator
        .generateFromTo[IO](sp, op)
        .flatMap {
          case Some(err) => IO.raiseError(new RuntimeException(err.intercalate("\n")))
          case None      => IO.pure(ExitCode.Success)
        }
    }
}
