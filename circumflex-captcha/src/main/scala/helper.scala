/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package ru.circumflex.captcha

import com.octo.captcha.service.image.DefaultManageableImageCaptchaService
import core.{ContextAware, HttpResponse, DirectStreamResponse}
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
    DirectStreamResponse(out => ImageIO.write(img, captchaFormat, out))
  }

  def captchaParamName = "_captcha"

  def captchaPassed: Boolean = captchaService
      .validateResponseForID(captchaId, ctx.stringParam(captchaParamName))
      .asInstanceOf[Boolean]
}