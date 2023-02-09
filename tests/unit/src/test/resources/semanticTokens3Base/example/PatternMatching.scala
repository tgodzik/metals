<<package>>/*keyword*/ <<example>>/*namespace*/

<<class>>/*keyword*/ <<PatternMatching>>/*class*/ {
  <<val>>/*keyword*/ <<some>>/*variable,readonly*/ = <<Some>>/*class*/(<<1>>/*number*/)
  <<some>>/*variable,readonly*/ <<match>>/*keyword*/ {
    <<case>>/*keyword*/ <<Some>>/*class*/(<<number>>/*variable,readonly*/) <<=>>>/*operator*/
      number
  }

  <<// tuple deconstruction>>/*comment*/
  <<val>>/*keyword*/ (<<left>>/*variable,readonly*/, <<right>>/*variable,readonly*/) = (<<1>>/*number*/, <<2>>/*number*/)
  (left, right)

  <<// val deconstruction>>/*comment*/
  <<val>>/*keyword*/ <<Some>>/*class*/(<<number1>>/*variable,readonly*/) =
    <<some>>/*variable,readonly*/
  println(number1)

  <<def>>/*keyword*/ localDeconstruction = {
    <<val>>/*keyword*/ Some(number2) =
      some
    <<number2>>/*variable,readonly*/
  }
}