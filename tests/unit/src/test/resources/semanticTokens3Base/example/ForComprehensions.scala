<<package>>/*keyword*/ <<example>>/*namespace*/

<<class>>/*keyword*/ <<ForComprehensions>>/*class*/ {
  <<for>>/*keyword*/ {
    a <<<->>/*operator*/ <<List>>/*variable*/(<<1>>/*number*/)
    b <<<->>/*operator*/ List(a)
    <<if>>/*keyword*/ (
      a,
      b,
    ) == (<<1>>/*number*/, <<2>>/*number*/)
    (
      c,
      d,
    ) <<<->>/*operator*/ List((a, b))
    <<if>>/*keyword*/ (
      a,
      b,
      c,
      d,
    ) == (<<1>>/*number*/, <<2>>/*number*/, <<3>>/*number*/, <<4>>/*number*/)
    e = (
      a,
      b,
      c,
      d,
    )
    <<if>>/*keyword*/ e == (<<1>>/*number*/, <<2>>/*number*/, <<3>>/*number*/, <<4>>/*number*/)
    f <<<->>/*operator*/ List(e)
  } <<yield>>/*keyword*/ {
    (
      a,
      b,
      c,
      d,
      e,
      f,
    )
  }

}