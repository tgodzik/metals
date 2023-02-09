<<package>>/*keyword*/ <<example>>/*namespace*/

<<import>>/*keyword*/ <<io>>/*namespace*/.<<circe>>/*namespace*/.<<derivation>>/*namespace*/.deriveDecoder
<<import>>/*keyword*/ <<io>>/*namespace*/.<<circe>>/*namespace*/.<<derivation>>/*namespace*/.deriveEncoder

<<class>>/*keyword*/ <<Definitions>>/*class*/ {
  <<Predef>>/*class*/.any2stringadd(<<1>>/*number*/)
  <<List>>/*variable*/[
    <<java>>/*namespace*/.<<util>>/*namespace*/.<<Map>>/*class*/.<<Entry>>/*interface,abstract*/[
      <<java>>/*namespace*/.<<lang>>/*namespace*/.<<Integer>>/*class*/,
      <<java>>/*namespace*/.<<lang>>/*namespace*/.<<Double>>/*class*/,
    ]
  ](
    elems = <<null>>/*keyword*/
  )
  println(deriveDecoder[MacroAnnotation])
  println(deriveEncoder[MacroAnnotation])
}