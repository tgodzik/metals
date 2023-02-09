<<package>>/*keyword*/ <<example>>/*namespace*/

<<class>>/*keyword*/ <<ImplicitConversions>>/*class*/ {
  <<implicit>>/*modifier*/ <<def>>/*keyword*/ string2Number(
      <<string>>/*parameter*/: <<String>>/*type*/
  ): <<Int>>/*class,abstract*/ = <<42>>/*number*/
  <<val>>/*keyword*/ <<message>>/*variable,readonly*/ = <<"">>/*string*/
  <<val>>/*keyword*/ <<number>>/*variable,readonly*/ = <<42>>/*number*/
  <<val>>/*keyword*/ <<tuple>>/*variable,readonly*/ = (<<1>>/*number*/, <<2>>/*number*/)
  <<val>>/*keyword*/ <<char>>/*variable,readonly*/: <<Char>>/*class,abstract*/ = <<'a'>>/*string*/

  <<// extension methods>>/*comment*/
  <<message>>/*variable,readonly*/
    .stripSuffix(<<"h">>/*string*/)
  tuple + <<"Hello">>/*string*/

  <<// implicit conversions>>/*comment*/
  <<val>>/*keyword*/ <<x>>/*variable,readonly*/: <<Int>>/*class,abstract*/ = message

  <<// interpolators>>/*comment*/
  <<s>>/*keyword*/<<">>/*string*/<<Hello >>/*string*/<<$>>/*keyword*/message<< >>/*string*/<<$>>/*keyword*/number<<">>/*string*/
  <<s>>/*keyword*/<<""">>/*string*/<<Hello>>/*string*/
<<     |>>/*string*/<<$>>/*keyword*/message<<>>/*string*/
<<     |>>/*string*/<<$>>/*keyword*/number<<""">>/*string*/.<<stripMargin>>/*variable,readonly*/

  <<val>>/*keyword*/ <<a>>/*variable,readonly*/: <<Int>>/*class,abstract*/ = char
  <<val>>/*keyword*/ <<b>>/*variable,readonly*/: <<Long>>/*class,abstract*/ = char
}