<<package>>/*keyword*/ <<example>>/*namespace*/.<<nested>>/*namespace*/

<<trait>>/*keyword*/ <<LocalDeclarations>>/*interface*/ {
  <<def>>/*keyword*/ foo(): <<Unit>>/*class,abstract*/
}

<<trait>>/*keyword*/ <<Foo>>/*interface*/ {
  <<val>>/*keyword*/ <<y>>/*variable,readonly*/ = <<3>>/*number*/
}

<<object>>/*keyword*/ <<LocalDeclarations>>/*class*/ {
  <<def>>/*keyword*/ create(): <<LocalDeclarations>>/*interface*/ = {
    <<def>>/*keyword*/ bar(): Unit = ()

    <<val>>/*keyword*/ x = <<new>>/*keyword*/ {
      <<val>>/*keyword*/ x = <<2>>/*number*/
    }

    <<val>>/*keyword*/ y = <<new>>/*keyword*/ Foo {}

    x.x + y.y

    <<new>>/*keyword*/ LocalDeclarations <<with>>/*keyword*/ Foo {
      <<override>>/*modifier*/ <<def>>/*keyword*/ foo(): Unit = bar()
    }

  }
}