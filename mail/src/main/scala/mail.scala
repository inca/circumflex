package pro.savant.circumflex
package mail

import core._, freemarker._
import javax.mail.{Message, Session => MailSession}
import java.util.Properties
import javax.mail.internet.{AddressException, MimeMessage, InternetAddress}

case class Email(to: String, subject: String, html: String) {
  def prepare() = new MailMessage()
      .addTo(to)
      .setSubject(subject)
      .setHtml(html)
}
case class ScheduledEmail(email: Email, attempts: Int)

object MailWorker {
  val enabled = cx.get("mail.enabled").map(_.toString.toBoolean).getOrElse(true)
  val smtpHost = cx("mail.host").toString
  val smtpPort = cx("mail.port").toString
  val smtpUser = cx("mail.user").toString
  val smtpPassword = cx("mail.password").toString
  val useTLS = cx.get("mail.tls").map(_.toString.toBoolean).getOrElse(false)

  val props = new Properties
  props.put("mail.smtp.host", smtpHost)
  props.put("mail.smtp.port", smtpPort)
  props.put("mail.smtp.auth", "true")
  props.put("mail.smtp.user", smtpUser)
  props.put("mail.smtp.password", smtpPassword)
  if (useTLS) {
    props.put("mail.smtp.starttls.enable","true")
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
        t.sendMessage(m.mimeMessage, m.mimeMessage.getAllRecipients)
      } catch {
        case e: Exception => failedMessages ++= List(m)
      })
      failedMessages
    } finally {
      t.close()
    }
  } else Nil
}

class MailMessage {
  import MailWorker._

  val fromAddress = new InternetAddress(
    MailWorker.fromAddress, MailWorker.fromTitle, "UTF-8")

  val mimeMessage = new MimeMessage(session)
  mimeMessage.setFrom(fromAddress)

  def recipients: Seq[InternetAddress] = mimeMessage.getAllRecipients.flatMap {
    case a: InternetAddress => Some(a)
    case _ => None
  }

  def addTo(addresses: String*): this.type = {
    addresses.map(a =>
      mimeMessage.addRecipient(Message.RecipientType.TO, new InternetAddress(a)))
    this
  }
  def setSubject(subject: String): this.type = {
    mimeMessage.setSubject(subject, "UTF-8")
    this
  }
  def setHtml(html: String): this.type = {
    mimeMessage.setContent(html, "text/html; charset=\"UTF-8\"")
    this
  }
  def send() {
    if (enabled) {
      val t = session.getTransport("smtp")
      try {
        t.connect(smtpHost, smtpUser, smtpPassword)
        t.sendMessage(mimeMessage, mimeMessage.getAllRecipients)
      } finally {
        t.close()
      }
    } else {
      MAIL_LOG.warn("Email sending has been disabled.")
      MAIL_LOG.debug("Logging disabled message.\n\n" + mimeMessage.getContent)
    }
  }
}

class MailSender extends TaskManager {

  def schedule(to: String, subjectKey: String) {
    val subject = msg.getOrElse(subjectKey, subjectKey)
    val html = ftl2string("/email.ftl")
    schedule(Email(to, subject, html))
  }

  def schedule(message: Email) {
    _reschedule(message, 1)
  }

  protected def _reschedule(message: Email, attempt: Int) {
    enqueue { () =>
      if (attempt > MailWorker.maxAttempts)
        MAIL_LOG.warn("Ran out of attempts while trying to send email to " + message.to)
      else try {
        message.prepare().send()
      } catch {
        case e: AddressException =>
          MAIL_LOG.warn("Could not send message to " + message.to)
        case e: Exception =>
          MAIL_LOG.trace("Failed to send a message, rescheduling.", e)
          _reschedule(message, attempt + 1)
      }
    }
  }
}
