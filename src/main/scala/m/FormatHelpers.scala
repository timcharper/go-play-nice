package m

object FormatHelpers {
  def indentText(s: String, n: Int, firstLineToo: Boolean = true): String = {
    val idt = " " * n
    if (firstLineToo)
      idt + s.replaceAll("\n", "\n" + idt)
    else
      s.replaceAll("\n", "\n" + idt)
  }

  def indent(n: Int): String => String =
    (" " * n) + (_ : String)
}

