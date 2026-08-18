/*
 * Copyright 2026 Werner Stein
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
package ca.uwaterloo.flix.tools.pkg.github

import java.io.{ByteArrayInputStream, IOException, InputStream}
import java.net.URI
import java.net.http.{HttpClient, HttpHeaders, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.util.Optional
import javax.net.ssl.SSLSession
import scala.jdk.CollectionConverters.*

/**
  * A single scripted answer for [[FakeTransport]]. `failAfterBytes`, when set, makes the body throw
  * [[IOException]] after that many bytes, standing in for a connection dropped mid-download.
  */
case class CannedResponse(
  status: Int,
  headers: Map[String, String] = Map.empty,
  body: String = "",
  failAfterBytes: Option[Int] = None
)

/**
  * Builds a [[GitHub.Transport]] that answers from a fixed script instead of the network.
  *
  * `script` maps a request URI to the responses it should give, in order. A URI that runs out of
  * queued responses -- or was never given any -- fails the test loudly rather than falling through
  * to a real network call.
  */
object FakeTransport {

  def apply(script: Map[String, List[CannedResponse]]): GitHub.Transport = {
    var remaining = script
    GitHub.Transport { request =>
      val uri = request.uri().toString
      remaining.get(uri) match {
        case Some(next :: rest) =>
          remaining = remaining.updated(uri, rest)
          toHttpResponse(request, next)
        case Some(Nil) =>
          throw new AssertionError(s"FakeTransport's script for $uri is exhausted")
        case None =>
          throw new AssertionError(s"FakeTransport has no response scripted for $uri")
      }
    }
  }

  private def toHttpResponse(req: HttpRequest, canned: CannedResponse): HttpResponse[InputStream] =
    new HttpResponse[InputStream] {
      override def statusCode(): Int = canned.status
      override def request(): HttpRequest = req
      override def previousResponse(): Optional[HttpResponse[InputStream]] = Optional.empty()
      override def headers(): HttpHeaders = {
        val raw = canned.headers.view.mapValues(v => java.util.List.of(v)).toMap.asJava
        HttpHeaders.of(raw, (_, _) => true)
      }
      override def body(): InputStream = {
        val bytes = new ByteArrayInputStream(canned.body.getBytes(StandardCharsets.UTF_8))
        canned.failAfterBytes.fold[InputStream](bytes)(new FailingAfterStream(bytes, _))
      }
      override def sslSession(): Optional[SSLSession] = Optional.empty()
      override def uri(): URI = req.uri()
      override def version(): HttpClient.Version = HttpClient.Version.HTTP_1_1
    }

  /**
    * Wraps `delegate`, throwing [[IOException]] once `budget` bytes have been read from it.
    */
  private class FailingAfterStream(delegate: InputStream, budget: Int) extends InputStream {
    private var remaining = budget

    override def read(): Int = {
      if (remaining <= 0) throw new IOException("FakeTransport: simulated connection drop")
      remaining -= 1
      delegate.read()
    }
  }
}
