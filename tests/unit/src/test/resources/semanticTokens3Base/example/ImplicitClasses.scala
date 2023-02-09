<<package>>/*keyword*/ <<example>>/*namespace*/

<<object>>/*keyword*/ <<ImplicitClasses>>/*class*/ {
  <<implicit>>/*modifier*/ <<class>>/*keyword*/ <<Xtension>>/*class*/(<<number>>/*variable,readonly*/: <<Int>>/*class,abstract*/) {
    <<def>>/*keyword*/ increment: <<Int>>/*class,abstract*/ = number + <<1>>/*number*/
  }
  <<implicit>>/*modifier*/ <<class>>/*keyword*/ <<XtensionAnyVal>>/*class*/(<<private>>/*modifier*/ <<val>>/*keyword*/ <<number>>/*variable,readonly*/: <<Int>>/*class,abstract*/) <<extends>>/*keyword*/ AnyVal {
    <<def>>/*keyword*/ double: <<Int>>/*class,abstract*/ = number * <<2>>/*number*/
  }
}