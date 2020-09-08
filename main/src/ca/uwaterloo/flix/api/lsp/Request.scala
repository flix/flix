/*
 * Copyright 2020 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.api.lsp

import java.nio.file.{Path, Paths}

import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.json4s
import org.json4s.JsonAST.{JInt, JString, JValue}

/**
  * A common super-type for language server requests.
  */
sealed trait Request {
  /**
    * A unique number that identifies this specific request.
    */
  def requestId: Int
}

object Request {

  /**
    * A request to add (or update) the given uri with the given source code.
    */
  case class AddUri(requestId: Int, uri: String, src: String) extends Request

  /**
    * A request to remove the given uri.
    */
  case class RemUri(requestId: Int, uri: String) extends Request

  /**
    * A request to shutdown the language server.
    */
  case object Shutdown extends Request {
    def requestId: Int = -1
  }

  /**
    * A request to run all benchmarks using the added URIs.
    */
  case class RunBenchmarks(requestId: Int) extends Request

  /**
    * A request to run main using the added URIs.
    */
  case class RunMain(requestId: Int) extends Request

  /**
    * A request to run all tests using the added URIs.
    */
  case class RunTests(requestId: Int) extends Request

  /**
    * A request to compile and check all source files.
    */
  case class Check(requestId: Int) extends Request

  /**
    * A request to get the type and effect of an expression.
    */
  case class Context(requestId: Int, uri: String, pos: Position) extends Request

  /**
    * A code lens request.
    */
  case class Codelens(requestId: Int, uri: String) extends Request

  /**
    * A request for code completion.
    */
  case class Complete(requestId: Int, uri: String, pos: Position) extends Request

  /**
    * A request to go to a declaration.
    */
  case class Goto(requestId: Int, uri: String, pos: Position) extends Request

  /**
    * A folding range request.
    */
  case class FoldingRange(requestId: Int, uri: String) extends Request

  /**
    * A request for all symbols.
    */
  case class Symbols(requestId: Int, uri: String) extends Request

  /**
    * A request to find all uses of an entity.
    */
  case class Uses(requestId: Int, uri: String, pos: Position) extends Request

  /**
    * A request to run all benchmarks in the project.
    */
  case class PackageBenchmark(requestId: Int, projectRoot: Path) extends Request

  /**
    * A request to build the project.
    */
  case class PackageBuild(requestId: Int, projectRoot: Path) extends Request

  /**
    * A request to build the project documentation.
    */
  case class PackageBuildDoc(requestId: Int, projectRoot: Path) extends Request

  /**
    * A request to build the JAR from the project.
    */
  case class PackageBuildJar(requestId: Int, projectRoot: Path) extends Request

  /**
    * A request to build a Flix package from the project.
    */
  case class PackageBuildPkg(requestId: Int, projectRoot: Path) extends Request

  /**
    * A request to init a new project.
    */
  case class PackageInit(requestId: Int, projectRoot: Path) extends Request

  /**
    * A request to run all tests in the project.
    */
  case class PackageTest(requestId: Int, projectRoot: Path) extends Request

  /**
    * Tries to parse the given `json` value as a [[AddUri]] request.
    */
  def parseAddUri(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    val srcRes: Result[String, String] = json \\ "src" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected src: '$s'.")
    }
    for {
      id <- idRes
      uri <- uriRes
      src <- srcRes
    } yield Request.AddUri(id, uri, src)
  }

  /**
    * Tries to parse the given `json` value as a [[RemUri]] request.
    */
  def parseRemUri(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      uri <- uriRes
    } yield Request.RemUri(id, uri)
  }

  /**
    * Tries to parse the given `json` value as a [[Complete]] request.
    */
  def parseComplete(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      uri <- uriRes
      pos <- Position.parse(json \\ "position")
    } yield Request.Complete(id, uri, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Context]] request.
    */
  def parseContext(json: JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      uri <- uriRes
      pos <- Position.parse(json \\ "position")
    } yield Request.Context(id, uri, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Check]] request.
    */
  def parseCheck(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    for {
      id <- idRes
    } yield Request.Check(id)
  }

  /**
    * Tries to parse the given `json` value as a [[Codelens]] request.
    */
  def parseCodelens(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      uri <- uriRes
    } yield Request.Codelens(id, uri)
  }

  /**
    * Tries to parse the given `json` value as a [[FoldingRange]] request.
    */
  def parseFoldingRange(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      uri <- uriRes
    } yield Request.FoldingRange(id, uri)
  }

  /**
    * Tries to parse the given `json` value as a [[Goto]] request.
    */
  def parseGoto(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      uri <- uriRes
      pos <- Position.parse(json \\ "position")
    } yield Request.Goto(id, uri, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Symbols]] request.
    */
  def parseSymbols(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      uri <- uriRes
    } yield Request.Symbols(id, uri)
  }

  /**
    * Tries to parse the given `json` value as a [[Uses]] request.
    */
  def parseUses(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      uri <- uriRes
      pos <- Position.parse(json \\ "position")
    } yield Request.Uses(id, uri, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[RunBenchmarks]] request.
    */
  def parseRunBenchmarks(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    for {
      id <- idRes
    } yield Request.RunBenchmarks(id)
  }

  /**
    * Tries to parse the given `json` value as a [[RunMain]] request.
    */
  def parseRunMain(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    for {
      id <- idRes
    } yield Request.RunMain(id)
  }

  /**
    * Tries to parse the given `json` value as a [[RunTests]] request.
    */
  def parseRunTests(json: json4s.JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    for {
      id <- idRes
    } yield Request.RunTests(id)
  }

  /**
    * Tries to parse the given `json` value as a [[PackageBenchmark]] request.
    */
  def parsePackageBenchmark(json: JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val projectRootUri: Result[String, String] = json \\ "projectRoot" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      projectRoot <- projectRootUri
    } yield Request.PackageBenchmark(id, Paths.get(projectRoot))
  }

  /**
    * Tries to parse the given `json` value as a [[PackageBuild]] request.
    */
  def parsePackageBuild(json: JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val projectRootUri: Result[String, String] = json \\ "projectRoot" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      projectRoot <- projectRootUri
    } yield Request.PackageBuild(id, Paths.get(projectRoot))
  }

  /**
    * Tries to parse the given `json` value as a [[PackageBuildDoc]] request.
    */
  def parsePackageBuildDoc(json: JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val projectRootUri: Result[String, String] = json \\ "projectRoot" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      projectRoot <- projectRootUri
    } yield Request.PackageBuildDoc(id, Paths.get(projectRoot))
  }

  /**
    * Tries to parse the given `json` value as a [[PackageBuildJar]] request.
    */
  def parsePackageBuildJar(json: JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val projectRootUri: Result[String, String] = json \\ "projectRoot" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      projectRoot <- projectRootUri
    } yield Request.PackageBuildJar(id, Paths.get(projectRoot))
  }

  /**
    * Tries to parse the given `json` value as a [[PackageBuildPkg]] request.
    */
  def parsePackageBuildPkg(json: JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val projectRootUri: Result[String, String] = json \\ "projectRoot" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      projectRoot <- projectRootUri
    } yield Request.PackageBuildPkg(id, Paths.get(projectRoot))
  }

  /**
    * Tries to parse the given `json` value as a [[PackageInit]] request.
    */
  def parsePackageInit(json: JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val projectRootUri: Result[String, String] = json \\ "projectRoot" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      projectRoot <- projectRootUri
    } yield Request.PackageInit(id, Paths.get(projectRoot))
  }

  /**
    * Tries to parse the given `json` value as a [[PackageTest]] request.
    */
  def parsePackageTest(json: JValue): Result[Request, String] = {
    val idRes: Result[Int, String] = json \\ "uri" match {
      case JInt(i) => Ok(i.toInt)
      case s => Err(s"Unexpected id: '$s'.")
    }
    val projectRootUri: Result[String, String] = json \\ "projectRoot" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      id <- idRes
      projectRoot <- projectRootUri
    } yield Request.PackageTest(id, Paths.get(projectRoot))
  }

}
