package ru.circumflex.web

import org.apache._
import http.client.entity.UrlEncodedFormEntity
import http.Header
import http.impl.client.DefaultHttpClient
import http.message.BasicNameValuePair
import http.util.EntityUtils
import http.client.{methods => m}
import collection.JavaConversions._
import collection.mutable.ListBuffer

/*!# Testing your application

Circumflex Web Framework lets you test your web application using the `MockApp`.

Refer to our test sources at [`circumflex-web/src/test/scala`][tests] to see it in action.

   [tests]: http://github.com/inca/circumflex/tree/master/circumflex-web/src/test/scala/
*/
trait MockServer extends StandaloneServer {
  def baseUrl = "http://localhost:" + port
  def conversate[A](c: MockConversation => A): A = {
    val conv = new MockConversation(this)
    c(conv)
  }
  def get(uri: String) = conversate(_.get(uri))
  def post(uri: String) = conversate(_.post(uri))
  def put(uri: String) = conversate(_.put(uri))
  def delete(uri: String) = conversate(_.delete(uri))
  def head(uri: String) = conversate(_.head(uri))
  def options(uri: String) = conversate(_.options(uri))
}

object MockApp extends MockServer

class MockConversation(val server: MockServer) {
  val baseUrl = server.baseUrl
  val client = new DefaultHttpClient()
  def get(uri: String) = new MockRequest(this, new m.HttpGet(baseUrl + uri))
  def post(uri: String) = new MockRequest(this, new m.HttpPost(baseUrl + uri))
  def put(uri: String) = new MockRequest(this, new m.HttpPut(baseUrl + uri))
  def delete(uri: String) = new MockRequest(this, new m.HttpDelete(baseUrl + uri))
  def head(uri: String) = new MockRequest(this, new m.HttpHead(baseUrl + uri))
  def options(uri: String) = new MockRequest(this, new m.HttpOptions(baseUrl + uri))
  // patch is still not supported by apache httpclient =(
}

class MockRequest(val conv: MockConversation, val req: m.HttpRequestBase) {
  val params = new ListBuffer[(String, String)]
  def setHeader(name: String, value: String): this.type = {
    req.setHeader(name, value)
    return this
  }
  def setParam(name: String, value: String): this.type = {
    params += name -> value
    return this
  }
  def execute() = {
    // apply params if applicable
    req match {
      case req: m.HttpEntityEnclosingRequestBase =>
        val pairs = params.map(p => new BasicNameValuePair(p._1, p._2)).toList
        val e = new UrlEncodedFormEntity(seqAsJavaList(pairs))
        req.setEntity(e)
      case _ =>
    }
    new MockResponse(conv, conv.client.execute(req))
  }
  override def toString = req.getRequestLine.toString
}

class MockResponse(val conv: MockConversation, val res: http.HttpResponse) {
  def statusCode = {
    println(res.getStatusLine.getStatusCode)
    println(res.getStatusLine)
    println(res)
    res.getStatusLine.getStatusCode
  }
  def content = EntityUtils.toString(res.getEntity)
  def headers(name: String): Seq[Header] = res.getHeaders(name).toSeq
  override def toString = res.getStatusLine.toString
}
