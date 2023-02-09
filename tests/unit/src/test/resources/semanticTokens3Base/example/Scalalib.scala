<<package>>/*keyword*/ <<example>>/*namespace*/

<<class>>/*keyword*/ <<Scalalib>>/*class*/ {
  <<val>>/*keyword*/ <<nil>>/*variable,readonly*/ = <<List>>/*variable*/()
  <<val>>/*keyword*/ <<lst>>/*variable,readonly*/ = <<List>>/*variable*/[
    (
        Nothing,
        Null,
        Singleton,
        Any,
        AnyRef,
        AnyVal,
        Int,
        Short,
        Double,
        Float,
        Char,
    )
  ](<<null>>/*keyword*/)
  <<lst>>/*variable,readonly*/.<<isInstanceOf>>/*variable,readonly*/[<<Any>>/*class,abstract*/]
  <<lst>>/*variable,readonly*/.<<asInstanceOf>>/*variable,readonly*/[<<Any>>/*class,abstract*/]
  println(lst.##)
  lst ne lst
  lst eq lst
  lst == lst
}