<<package>>/*keyword*/ <<example>>/*namespace*/

<<class>>/*keyword*/ <<Miscellaneous>>/*class*/ {
  <<// backtick identifier>>/*comment*/
  <<val>>/*keyword*/ <<`a b`>>/*variable,readonly*/ = <<42>>/*number*/

  <<// block with only wildcard value>>/*comment*/
  <<def>>/*keyword*/ apply(): <<Unit>>/*class,abstract*/ = {
    <<val>>/*keyword*/ <<_>>/*variable*/ = <<42>>/*number*/
  }
  <<// infix + inferred apply/implicits/tparams>>/*comment*/
  (List(<<1>>/*number*/)
    .map(<<_>>/*variable*/ + <<1>>/*number*/)
    ++
      List(<<3>>/*number*/))
}