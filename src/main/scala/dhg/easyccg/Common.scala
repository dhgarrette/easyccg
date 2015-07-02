package dhg.easyccg

import dhg.util._
import dhg.ccg.cat._

/**
 * @author dhg
 */
object Common {

  object StripCatOuterParens {
    def apply(in: String) = if (in.contains("/") || in.contains("\\")) in.slyce(1, -1) else in
    def apply(in: Cat) = in match { case c: ComplexCat => c.toString.slyce(1, -1); case c => c.toString }
    def unapply(in: String) = Some(apply(in))
    def unapply(in: Cat) = Some(apply(in))
  }

}
