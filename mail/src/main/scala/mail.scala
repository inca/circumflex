package circumflex
package mail

import core._, freemarker._
import collection.mutable.HashSet
import javax.mail.{Session => MailSession, Address, Message}
import java.util.Properties
import javax.mail.internet.{MimeMessage, InternetAddress}
import java.io.Serializable
import javax.mail.Message.RecipientType

case class Email(to: String, subject: String, html: String)
    extends Serializable {

  def prepare() = new MailMessage()
      .addTo(to)
      .setSubject(subject)
      .setHtml(html)

}

case class ScheduledEmail(email: Email, attempts: Int)
    extends Serializable

object MailWorker {
  val enabled = cx.get("mail.enabled").map(_.toString.toBoolean).getOrElse(true)
  val smtpHost = cx("mail.host").toString
  val smtpPort = cx("mail.port").toString
  val smtpUser = cx("mail.user").toString
  val smtpPassword = cx("mail.password").toString
  val useTLS = cx.get("mail.tls").map(_.toString.toBoolean).getOrElse(false)
  val trustSSL = cx.get("mail.ssl.trust").map(_.toString.toBoolean).getOrElse(false)

  val props = new Properties
  props.put("mail.smtp.host", smtpHost)
  props.put("mail.smtp.port", smtpPort)
  props.put("mail.smtp.auth", "true")
  props.put("mail.smtp.user", smtpUser)
  props.put("mail.smtp.password", smtpPassword)
  if (useTLS) {
    props.put("mail.smtp.starttls.enable","true")
  }
  if (trustSSL) {
    props.put("mail.smtp.ssl.trust", "*")
  }
  val session = MailSession.getInstance(props, null)

  val maxAttempts: Int = cx.get("mail.queue.maxAttemts")
      .map(_.toString.toInt).getOrElse(3)

  def fromAddress = ctx.getString("mail.from.address")
      .orElse(cx.getString("mail.from.address"))
      .getOrElse(throw new CircumflexException("Please configure the `mail.from.address` parameter."))

  def fromTitle = ctx.getString("mail.from.title")
      .orElse(msg.get("mail.from.title"))
      .getOrElse("Untitled")

  /**
   * Bulk sends specified `messages`, returns failed messages, if any.
   */
  def send(messages: MailMessage*): Seq[MailMessage] = if (enabled) {
    val t = session.getTransport("smtp")
    try {
      t.connect(smtpHost, smtpUser, smtpPassword)
      var failedMessages: Seq[MailMessage] = Nil
      messages.foreach(m => try {
        val msg = m.prepareMessage
        t.sendMessage(msg, msg.getAllRecipients)
      } catch {
        case e: Exception => failedMessages ++= List(m)
      })
      failedMessages
    } finally {
      t.close()
    }
  } else Nil
}

class MailMessage extends Serializable {
  import MailWorker._

  val fromAddress = new InternetAddress(
    MailWorker.fromAddress, MailWorker.fromTitle, "UTF-8")

  val recipientsTo = new HashSet[Address]
  val recipientsCc = new HashSet[Address]
  val recipientsBcc = new HashSet[Address]

  def addTo(addresses: String*): this.type = {
    addresses.map(a => recipientsTo += new InternetAddress(a))
    this
  }

  def addCc(addresses: String*): this.type = {
    addresses.map(a => recipientsCc += new InternetAddress(a))
    this
  }

  def addBcc(addresses: String*): this.type = {
    addresses.map(a => recipientsBcc += new InternetAddress(a))
    this
  }

  protected var subject = ""

  def setSubject(subject: String): this.type = {
    this.subject = msg.getOrElse(subject, subject)
    this
  }

  protected var html = ""

  def setHtml(html: String): this.type = {
    this.html = html
    this
  }

  def setFtl(template: String): this.type = {
    this.html = ftl2string(template)
    this
  }

  def prepareMessage = {
    val m = new MimeMessage(session)
    m.setFrom(fromAddress)
    m.addRecipients(RecipientType.TO, recipientsTo.toArray)
    m.addRecipients(RecipientType.CC, recipientsCc.toArray)
    m.addRecipients(RecipientType.BCC, recipientsBcc.toArray)
    m.setSubject(subject, "utf-8")
    m.setContent(html, "text/html; charset=\"UTF-8\"")
    m
  }

  def send() {
    if (enabled) {
      val m = prepareMessage
      val t = session.getTransport("smtp")
      try {
        t.connect(smtpHost, smtpUser, smtpPassword)
        t.sendMessage(m, m.getAllRecipients)
      } finally {
        t.close()
      }
    }
  }

}
