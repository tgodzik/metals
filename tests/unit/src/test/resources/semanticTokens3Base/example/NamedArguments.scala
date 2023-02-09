<<package>>/*keyword*/ <<example>>/*namespace*/

<<case>>/*keyword*/ <<class>>/*keyword*/ <<User>>/*class*/(
    <<name>>/*variable,readonly*/: <<String>>/*type*/ = {
      <<// assert default values have occurrences>>/*comment*/
      <<Map>>/*variable*/.<<toString>>/*method*/
    }
)
<<object>>/*keyword*/ <<NamedArguments>>/*class*/ {
  <<final>>/*modifier*/ <<val>>/*keyword*/ <<susan>>/*variable,readonly*/ = <<"Susan">>/*string*/
  <<val>>/*keyword*/ <<user1>>/*variable,readonly*/ =
    <<User>>/*class*/
      .<<apply>>/*method*/(
        <<name>>/*parameter*/ = <<"John">>/*string*/
      )
  <<val>>/*keyword*/ <<user2>>/*variable,readonly*/: <<User>>/*class*/ =
    <<User>>/*class*/(
      name = susan
    ).<<copy>>/*variable,readonly*/(
      name = susan
    )

  <<// anonymous classes>>/*comment*/
  <<@>>/*keyword*/<<deprecated>>/*class*/(
    message = <<"a">>/*string*/,
    since = susan,
  ) <<def>>/*keyword*/ b = <<1>>/*number*/

  <<// vararg>>/*comment*/
  <<List>>/*variable*/(
    elems = <<2>>/*number*/
  )

}