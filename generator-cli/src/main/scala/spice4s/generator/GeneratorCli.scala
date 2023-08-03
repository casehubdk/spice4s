/*
 * Copyright 2023 CaseHubDK
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
