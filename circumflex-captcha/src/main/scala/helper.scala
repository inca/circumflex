package ru.circumflex.captcha

import com.octo.captcha.engine.image.gimpy.DoubleRandomListGimpyEngine
import com.octo.captcha.service.image.{DefaultManageableImageCaptchaService, ImageCaptchaService}
import core.{ContextAware, HttpResponse, DirectStreamResponse}
import java.util.Properties
import javax.imageio.ImageIO

object CaptchaService extends DefaultManageableImageCaptchaService

trait CaptchaHelper extends ContextAware {

  def captchaService: DefaultManageableImageCaptchaService = CaptchaService

  def captchaId = ctx.request.getSession.getId

  def captchaFormat = "jpg";

  def renderCaptcha: HttpResponse = {
    val img = captchaService.getImageChallengeForID(captchaId, ctx.request.getLocale)
    ctx.statusCode = 200
    ctx.noCache()
    ctx.contentType = "image/" + captchaFormat
    DirectStreamResponse(ctx, out => ImageIO.write(img, captchaFormat, out))
  }

  def captchaParamName = "_captcha"

  def captchaPassed: Boolean = captchaService
      .validateResponseForID(captchaId, ctx.stringParam(captchaParamName))
      .asInstanceOf[Boolean]
}