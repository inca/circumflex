package circumflex
package markeven

object testRenderer extends MarkevenRenderer {

  def resolveMedia(id: String) = id match {
    case "acorn" => Some(
      new LinkDef("http://eduarea.com/img/acorn.png", "EduArea Acorn Logo"))
    case _ => None
  }

  def resolveLink(id: String) = id match {
    case "ea" => Some(new LinkDef("http://eduarea.com", "EduArea & Friends"))
    case "cx" => Some(new LinkDef("http://circumflex.ru"))
    case _ => None
  }

  def resolveFragment(id: String) = id match {
    case "normal" =>
      Some(new FragmentDef("normal *text* -- <em>process</em> it"))
    case "code" =>
      Some(new FragmentDef("*code*, <em>a & b</em>", ProcessingMode.CODE))
    case "plain" =>
      Some(new FragmentDef("*plain*, <em>a & b</em>", ProcessingMode.PLAIN))
    case "cyclic1" =>
      Some(new FragmentDef("= {{cyclic2}}", ProcessingMode.NORMAL))
    case "cyclic2" =>
      Some(new FragmentDef("== {{cyclic1}}", ProcessingMode.NORMAL))
    case _ => None
  }

}