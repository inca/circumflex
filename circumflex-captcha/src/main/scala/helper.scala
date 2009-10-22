package ru.circumflex.captcha

import com.octo.captcha.engine.image.gimpy.DoubleRandomListGimpyEngine
import com.octo.captcha.service.image.{DefaultManageableImageCaptchaService, ImageCaptchaService}
import core.{ContextAware, HttpResponse, DirectStreamResponse}
import java.util.Properties
import javax.imageio.ImageIO

object CaptchaService extends DefaultManageableImageCaptchaService

trait CaptchaHelper extends ContextAware {

  def captchaService: DefaultManageableImageCaptchaService = CaptchaService

  def captchaId = routeContext.request.getSession.getId

  def captchaFormat = "jpg";

  def renderCaptcha: HttpResponse = {
    val img = captchaService.getService.getImageChallengeForID(captchaId, routeContext.request.getLocale)
    routeContext.statusCode = 200
    routeContext.noCache()
    routeContext.contentType = "image/" + captchaFormat
    DirectStreamResponse(routeContext, out => ImageIO.write(img, captchaFormat, out))
  }

  def captchaParamName = "_captcha"

  def captchaPassed: Boolean = captchaService.getService
      .validateResponseForID(captchaId, routeContext.stringParam(captchaParamName))
      .asInstanceOf[Boolean]
}