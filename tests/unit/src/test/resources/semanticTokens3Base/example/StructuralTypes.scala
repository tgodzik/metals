<<package>>/*keyword*/ <<example>>/*namespace*/

<<object>>/*keyword*/ <<StructuralTypes>>/*class*/ {
  <<type>>/*keyword*/ <<User>>/*type*/ = {
    <<def>>/*keyword*/ name: <<String>>/*type*/
    <<def>>/*keyword*/ age: <<Int>>/*class,abstract*/
  }

  <<val>>/*keyword*/ <<user>>/*variable,readonly*/ = <<null>>/*keyword*/.<<asInstanceOf>>/*method*/[<<User>>/*type*/]
  <<user>>/*variable,readonly*/.name
  <<user>>/*variable,readonly*/.age

  <<val>>/*keyword*/ <<V>>/*variable,readonly*/: <<Object>>/*class*/ {
    <<def>>/*keyword*/ scalameta: <<String>>/*type*/
  } = <<new>>/*keyword*/ {
    <<def>>/*keyword*/ scalameta = <<"4.0">>/*string*/
  }
  <<V>>/*variable,readonly*/.scalameta
}