package tests.tokens

import tests.BaseSemanticTokensSuite

class SemanticTokensScala3Suite extends BaseSemanticTokensSuite {
  override protected def ignoreScalaVersion: Option[IgnoreScalaVersion] = Some(
    IgnoreScala2
  )

  check(
    "extension",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<extension>>/*keyword*/ (<<i>>/*parameter*/: <<Int>>/*class,abstract*/)
        |  <<def>>/*keyword*/ <<asString>>/*method*/: <<String>>/*type*/ = <<i>>/*parameter*/.<<toString>>/*method*/
        |
        |<<extension>>/*keyword*/ (<<s>>/*parameter*/: <<String>>/*type*/) {
        |  <<def>>/*keyword*/ <<asInt>>/*method*/: <<Int>>/*class,abstract*/ = <<s>>/*parameter*/.<<toInt>>/*method*/
        |  <<def>>/*keyword*/ <<double>>/*method*/: <<String>>/*type*/ = <<s>>/*parameter*/ <<*>>/*method*/ <<2>>/*number*/
        |}
        |
        |<<trait>>/*keyword*/ <<AbstractExtension>>/*interface*/ {
        |  <<extension>>/*keyword*/ (<<d>>/*parameter*/: <<Double>>/*class,abstract*/) {
        |    <<def>>/*keyword*/ <<abc>>/*method*/: <<String>>/*type*/
        |  }
        |}
        |""".stripMargin,
  )

  check(
    "abstract-given",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<abstract>>/*modifier*/ <<class>>/*keyword*/ <<AbstractGiven>>/*class,abstract*/:
        |  <<given>>/*keyword*/ <<int>>/*method*/: <<Int>>/*class,abstract*/
        |
        |""".stripMargin,
  )

  check(
    "enum1",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<enum>>/*keyword*/ <<Color>>/*enum,abstract*/(<<val>>/*keyword*/ <<rgb>>/*variable,readonly*/: <<Int>>/*class,abstract*/):
        |   <<case>>/*keyword*/ <<Red>>/*enum*/   <<extends>>/*keyword*/ <<Color>>/*enum,abstract*/(<<0xFF0000>>/*number*/)
        |   <<case>>/*keyword*/ <<Green>>/*enum*/ <<extends>>/*keyword*/ <<Color>>/*enum,abstract*/(<<0x00FF00>>/*number*/)
        |   <<case>>/*keyword*/ <<Blue>>/*enum*/  <<extends>>/*keyword*/ <<Color>>/*enum,abstract*/(<<0x0000FF>>/*number*/)
        |
        |""".stripMargin,
  )

  check(
    "enum2",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<enum>>/*keyword*/ <<FooEnum>>/*enum,abstract*/:
        |  <<case>>/*keyword*/ <<Bar>>/*enum*/, <<Baz>>/*enum*/
        |<<object>>/*keyword*/ <<FooEnum>>/*class*/
        |""".stripMargin,
  )

  check(
    "enum3",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<enum>>/*keyword*/ <<FooEnum>>/*enum,abstract*/:
        |  <<case>>/*keyword*/ <<A>>/*enum*/(<<a>>/*variable,readonly*/: <<Int>>/*class,abstract*/)
        |  <<case>>/*keyword*/ <<B>>/*enum*/(<<a>>/*variable,readonly*/: <<Int>>/*class,abstract*/, <<b>>/*variable,readonly*/: <<Int>>/*class,abstract*/)
        |  <<case>>/*keyword*/ <<C>>/*enum*/(<<a>>/*variable,readonly*/: <<Int>>/*class,abstract*/, <<b>>/*variable,readonly*/: <<Int>>/*class,abstract*/, <<c>>/*variable,readonly*/: <<Int>>/*class,abstract*/)
        |
        |""".stripMargin,
  )

  check(
    "toplevel-val",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<def>>/*keyword*/ <<foo>>/*method*/(): <<Int>>/*class,abstract*/ = <<42>>/*number*/
        |
        |<<val>>/*keyword*/ <<abc>>/*variable,readonly*/: <<String>>/*type*/ = <<"sds">>/*string*/
        |
        |<<// tests jar's indexing on Windows>>/*comment*/
        |<<type>>/*keyword*/ <<SourceToplevelTypeFromDepsRef>>/*type*/ = <<EmptyTuple>>/*type*/
        |""".stripMargin,
  )

  check(
    "given-ord",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |<<trait>>/*keyword*/ <<Ord>>/*interface*/[<<T>>/*typeParameter,abstract*/]:
        |   <<def>>/*keyword*/ <<compare>>/*method*/(<<x>>/*parameter*/: <<T>>/*typeParameter,abstract*/, <<y>>/*parameter*/: <<T>>/*typeParameter,abstract*/): <<Int>>/*class,abstract*/
        |<<given>>/*keyword*/ <<intOrd>>/*class*/: <<Ord>>/*interface*/[<<Int>>/*class,abstract*/] <<with>>/*keyword*/
        |   <<def>>/*keyword*/ <<compare>>/*method*/(<<x>>/*parameter*/: <<Int>>/*class,abstract*/, <<y>>/*parameter*/: <<Int>>/*class,abstract*/) =
        |     <<if>>/*keyword*/ <<x>>/*parameter*/ <<<>>/*method*/ <<y>>/*parameter*/ <<then>>/*keyword*/ -<<1>>/*number*/ <<else>>/*keyword*/ <<if>>/*keyword*/ <<x>>/*parameter*/ <<>>>/*method*/ <<y>>/*parameter*/ <<then>>/*keyword*/ +<<1>>/*number*/ <<else>>/*keyword*/ <<0>>/*number*/
        |<<given>>/*keyword*/ <<Ord>>/*interface*/[<<String>>/*type*/] <<with>>/*keyword*/
        |   <<def>>/*keyword*/ <<compare>>/*method*/(<<x>>/*parameter*/: <<String>>/*type*/, <<y>>/*parameter*/: <<String>>/*type*/) =
        |     <<x>>/*parameter*/.<<compare>>/*method*/(<<y>>/*parameter*/)
        |""".stripMargin,
  )

  check(
    "and-or-type",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<trait>>/*keyword*/ <<Cancelable>>/*interface*/ 
        |<<trait>>/*keyword*/ <<Movable>>/*interface*/ 
        |
        |<<type>>/*keyword*/ <<Y>>/*type*/ = (<<Cancelable>>/*interface*/ <<&>>/*type*/ <<Movable>>/*interface*/)
        |
        |<<type>>/*keyword*/ <<X>>/*type*/ = <<String>>/*type*/ <<|>>/*type*/ <<Int>>/*class,abstract*/
        |""".stripMargin,
  )

  check(
    "given-big",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<given>>/*keyword*/ <<intValue>>/*variable,readonly*/: <<Int>>/*class,abstract*/ = <<4>>/*number*/
        |<<given>>/*keyword*/ <<String>>/*type*/ = <<"str">>/*string*/
        |<<given>>/*keyword*/ (<<using>>/*keyword*/ <<i>>/*parameter*/: <<Int>>/*class,abstract*/): <<Double>>/*class,abstract*/ = <<4.0>>/*number*/
        |<<given>>/*keyword*/ [<<T>>/*typeParameter,abstract*/]: <<List>>/*type*/[<<T>>/*typeParameter,abstract*/] = <<Nil>>/*variable*/
        |<<given>>/*keyword*/ <<given_Char>>/*variable,readonly*/: <<Char>>/*class,abstract*/ = <<'?'>>/*string*/
        |<<given>>/*keyword*/ <<`given_Float`>>/*variable,readonly*/: <<Float>>/*class,abstract*/ = <<3.0>>/*number*/
        |<<given>>/*keyword*/ <<`* *`>>/*variable,readonly*/: <<Long>>/*class,abstract*/ = <<5>>/*number*/
        |
        |<<def>>/*keyword*/ <<method>>/*method*/(<<using>>/*keyword*/ <<Int>>/*class,abstract*/) = <<"">>/*string*/
        |
        |<<object>>/*keyword*/ <<X>>/*class*/ {
        |  <<given>>/*keyword*/ <<Double>>/*class,abstract*/ = <<4.0>>/*number*/
        |  <<val>>/*keyword*/ <<double>>/*variable,readonly*/ = <<given_Double>>/*variable,readonly*/
        |
        |  <<given>>/*keyword*/ <<of>>/*method*/[<<A>>/*typeParameter,abstract*/]: <<Option>>/*class,abstract*/[<<A>>/*typeParameter,abstract*/] = <<???>>/*method*/
        |}
        |
        |<<trait>>/*keyword*/ <<Xg>>/*interface*/:
        |  <<def>>/*keyword*/ <<doX>>/*method*/: <<Int>>/*class,abstract*/
        |
        |<<trait>>/*keyword*/ <<Yg>>/*interface*/:
        |  <<def>>/*keyword*/ <<doY>>/*method*/: <<String>>/*type*/
        |
        |<<trait>>/*keyword*/ <<Zg>>/*interface*/[<<T>>/*typeParameter,abstract*/]:
        |  <<def>>/*keyword*/ <<doZ>>/*method*/: <<List>>/*type*/[<<T>>/*typeParameter,abstract*/]
        |
        |<<given>>/*keyword*/ <<Xg>>/*interface*/ <<with>>/*keyword*/
        |  <<def>>/*keyword*/ <<doX>>/*method*/ = <<7>>/*number*/
        |
        |<<given>>/*keyword*/ (<<using>>/*keyword*/ <<Xg>>/*interface*/): <<Yg>>/*interface*/ <<with>>/*keyword*/
        |  <<def>>/*keyword*/ <<doY>>/*method*/ = <<"7">>/*string*/
        |
        |<<given>>/*keyword*/ [<<T>>/*typeParameter,abstract*/]: <<Zg>>/*interface*/[<<T>>/*typeParameter,abstract*/] <<with>>/*keyword*/
        |  <<def>>/*keyword*/ <<doZ>>/*method*/: <<List>>/*type*/[<<T>>/*typeParameter,abstract*/] = <<Nil>>/*variable*/
        |
        |<<val>>/*keyword*/ <<a>>/*variable,readonly*/ = <<intValue>>/*variable,readonly*/
        |<<val>>/*keyword*/ <<b>>/*variable,readonly*/ = <<given_String>>/*variable,readonly*/
        |<<val>>/*keyword*/ <<c>>/*variable,readonly*/ = <<X>>/*class*/.<<given_Double>>/*variable,readonly*/
        |<<val>>/*keyword*/ <<d>>/*variable,readonly*/ = <<given_List_T>>/*method*/[<<Int>>/*class,abstract*/]
        |<<val>>/*keyword*/ <<e>>/*variable,readonly*/ = <<given_Char>>/*variable,readonly*/
        |<<val>>/*keyword*/ <<f>>/*variable,readonly*/ = <<given_Float>>/*variable,readonly*/
        |<<val>>/*keyword*/ <<g>>/*variable,readonly*/ = <<`* *`>>/*variable,readonly*/
        |<<val>>/*keyword*/ <<i>>/*variable,readonly*/ = <<X>>/*class*/.<<of>>/*method*/[<<Int>>/*class,abstract*/]
        |<<val>>/*keyword*/ <<x>>/*variable,readonly*/ = <<given_Xg>>/*class*/
        |<<val>>/*keyword*/ <<y>>/*variable,readonly*/ = <<given_Yg>>/*method*/
        |<<val>>/*keyword*/ <<z>>/*variable,readonly*/ = <<given_Zg_T>>/*method*/[<<String>>/*type*/]
        |
        |""".stripMargin,
  )
  check(
    "named-arguments",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<case>>/*keyword*/ <<class>>/*keyword*/ <<User>>/*class*/(
        |    <<name>>/*variable,readonly*/: <<String>>/*type*/ = {
        |      <<// assert default values have occurrences>>/*comment*/
        |      <<Map>>/*variable*/.<<toString>>/*method*/
        |    }
        |)
        |<<object>>/*keyword*/ <<NamedArguments>>/*class*/ {
        |  <<final>>/*modifier*/ <<val>>/*keyword*/ <<susan>>/*variable,readonly*/ = <<"Susan">>/*string*/
        |  <<val>>/*keyword*/ <<user1>>/*variable,readonly*/ =
        |    <<User>>/*class*/
        |      .<<apply>>/*method*/(
        |        <<name>>/*parameter*/ = <<"John">>/*string*/
        |      )
        |  <<val>>/*keyword*/ <<user2>>/*variable,readonly*/: <<User>>/*class*/ =
        |    <<User>>/*class*/(
        |      <<name>>/*parameter*/ = <<susan>>/*variable,readonly*/
        |    ).<<copy>>/*method*/(
        |      <<name>>/*parameter*/ = <<susan>>/*variable,readonly*/
        |    )
        |
        |  <<// anonymous classes>>/*comment*/
        |  <<@>>/*keyword*/<<deprecated>>/*class*/(
        |    <<message>>/*parameter*/ = <<"a">>/*string*/,
        |    <<since>>/*parameter*/ = <<susan>>/*variable,readonly*/,
        |  ) <<def>>/*keyword*/ <<b>>/*method,deprecated*/ = <<1>>/*number*/
        |
        |
        |}
        |""".stripMargin,
  )

  check(
    "vars",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<object>>/*keyword*/ <<Vars>>/*class*/ {
        |  <<var>>/*keyword*/ <<a>>/*variable*/ = <<2>>/*number*/
        |
        |  <<a>>/*variable*/ = <<2>>/*number*/
        |
        |  <<Vars>>/*class*/.<<a>>/*variable*/ = <<3>>/*number*/
        |}
        |""".stripMargin,
  )

  check(
    "var-args",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<class>>/*keyword*/ <<VarArgs>>/*class*/ {
        |  <<def>>/*keyword*/ <<add>>/*method*/(<<a>>/*parameter*/: <<Int>>/*class,abstract*/*) = <<a>>/*parameter*/
        |  <<def>>/*keyword*/ <<add2>>/*method*/(<<a>>/*parameter*/: <<Seq>>/*type*/[<<Int>>/*class,abstract*/]*) = <<a>>/*parameter*/
        |}
        |""".stripMargin,
  )
  check(
    "type-parameter",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<class>>/*keyword*/ <<TypeParameters>>/*class*/[<<A>>/*typeParameter,abstract*/] {
        |  <<def>>/*keyword*/ <<method>>/*method*/[<<B>>/*typeParameter,abstract*/] = <<42>>/*number*/
        |  <<trait>>/*keyword*/ <<TraitParameter>>/*interface*/[<<C>>/*typeParameter,abstract*/]
        |  <<type>>/*keyword*/ <<AbstractTypeAlias>>/*type,abstract*/[<<D>>/*typeParameter,abstract*/]
        |  <<type>>/*keyword*/ <<TypeAlias>>/*type*/[<<E>>/*typeParameter,abstract*/] = <<List>>/*type*/[<<E>>/*typeParameter,abstract*/]
        |}
        |""".stripMargin,
  )
  check(
    "try-catch",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<class>>/*keyword*/ <<TryCatch>>/*class*/ {
        |  <<try>>/*keyword*/ {
        |    <<val>>/*keyword*/ <<x>>/*variable,readonly*/ = <<2>>/*number*/
        |    <<x>>/*variable,readonly*/ <<+>>/*method*/ <<2>>/*number*/
        |  } <<catch>>/*keyword*/ {
        |    <<case>>/*keyword*/ <<t>>/*variable,readonly*/: <<Throwable>>/*type*/ <<=>>>/*operator*/
        |      <<t>>/*variable,readonly*/.<<printStackTrace>>/*method*/()
        |  } <<finally>>/*keyword*/ {
        |    <<val>>/*keyword*/ <<text>>/*variable,readonly*/ = <<"">>/*string*/
        |    <<text>>/*variable,readonly*/ <<+>>/*method*/ <<"">>/*string*/
        |  }
        |}
        |""".stripMargin,
  )

  check(
    "scalalib",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<class>>/*keyword*/ <<Scalalib>>/*class*/ {
        |  <<val>>/*keyword*/ <<nil>>/*variable,readonly*/ = <<List>>/*variable*/()
        |  <<val>>/*keyword*/ <<lst>>/*variable,readonly*/ = <<List>>/*variable*/[
        |    (
        |        <<Nothing>>/*class,abstract*/,
        |        <<Null>>/*class,abstract*/,
        |        <<Singleton>>/*interface*/,
        |        <<Any>>/*class,abstract*/,
        |        <<AnyRef>>/*type*/,
        |        <<AnyVal>>/*class,abstract*/,
        |        <<Int>>/*class,abstract*/,
        |        <<Short>>/*class,abstract*/,
        |        <<Double>>/*class,abstract*/,
        |        <<Float>>/*class,abstract*/,
        |        <<Char>>/*class,abstract*/
        |    )
        |  ](<<null>>/*keyword*/)
        |  <<lst>>/*variable,readonly*/.<<isInstanceOf>>/*method*/[<<Any>>/*class,abstract*/]
        |  <<lst>>/*variable,readonly*/.<<asInstanceOf>>/*method*/[<<Any>>/*class,abstract*/]
        |  <<println>>/*method*/(<<lst>>/*variable,readonly*/.<<##>>/*method*/)
        |  <<lst>>/*variable,readonly*/ <<ne>>/*method*/ <<lst>>/*variable,readonly*/
        |  <<lst>>/*variable,readonly*/ <<eq>>/*method*/ <<lst>>/*variable,readonly*/
        |  <<lst>>/*variable,readonly*/ <<==>>/*method*/ <<lst>>/*variable,readonly*/
        |}""".stripMargin,
  )

  check(
    "pattern-matching",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<class>>/*keyword*/ <<PatternMatching>>/*class*/ {
        |  <<val>>/*keyword*/ <<some>>/*variable,readonly*/ = <<Some>>/*class*/(<<1>>/*number*/)
        |  <<some>>/*variable,readonly*/ <<match>>/*keyword*/ {
        |    <<case>>/*keyword*/ <<Some>>/*class*/(<<number>>/*variable,readonly*/) <<=>>>/*operator*/
        |      <<number>>/*variable,readonly*/
        |  }
        |
        |  <<// tuple deconstruction>>/*comment*/
        |  <<val>>/*keyword*/ (<<left>>/*variable,readonly*/, <<right>>/*variable,readonly*/) = (<<1>>/*number*/, <<2>>/*number*/)
        |  (<<left>>/*variable,readonly*/, <<right>>/*variable,readonly*/)
        |
        |  <<// val deconstruction>>/*comment*/
        |  <<val>>/*keyword*/ <<Some>>/*class*/(<<number1>>/*variable,readonly*/) =
        |    <<some>>/*variable,readonly*/
        |  <<println>>/*method*/(<<number1>>/*variable,readonly*/)
        |
        |  <<def>>/*keyword*/ <<localDeconstruction>>/*method*/ = {
        |    <<val>>/*keyword*/ <<Some>>/*class*/(<<number2>>/*variable,readonly*/) =
        |      <<some>>/*variable,readonly*/
        |    <<number2>>/*variable,readonly*/
        |  }
        |}
        |""".stripMargin,
  )
  check(
    "package",
    s"""|<<package>>/*keyword*/ <<object>>/*keyword*/ <<example>>/*class*/ {
        |
        |  <<class>>/*keyword*/ <<PackageObjectClass>>/*class*/
        |}
        |""".stripMargin,
  )

  check(
    "misscelaneous",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<class>>/*keyword*/ <<Miscellaneous>>/*class*/ {
        |  <<// backtick identifier>>/*comment*/
        |  <<val>>/*keyword*/ <<`a b`>>/*variable,readonly*/ = <<42>>/*number*/
        |
        |  <<// block with only wildcard value>>/*comment*/
        |  <<def>>/*keyword*/ <<apply>>/*method*/(): <<Unit>>/*class,abstract*/ = {
        |    <<val>>/*keyword*/ <<_>>/*variable*/ = <<42>>/*number*/
        |  }
        |  <<// infix + inferred apply/implicits/tparams>>/*comment*/
        |  (<<List>>/*variable*/(<<1>>/*number*/)
        |    .<<map>>/*method*/(<<_>>/*variable*/ <<+>>/*method*/ <<1>>/*number*/)
        |    <<++>>/*method*/
        |      <<List>>/*variable*/(<<3>>/*number*/))
        |}
        |""".stripMargin,
  )
  check(
    "method-overload",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<class>>/*keyword*/ <<MethodOverload>>/*class*/(<<b>>/*variable,readonly*/: <<String>>/*type*/) {
        |  <<def>>/*keyword*/ <<this>>/*keyword*/() = <<this>>/*keyword*/(<<"">>/*string*/)
        |  <<def>>/*keyword*/ <<this>>/*keyword*/(<<c>>/*parameter*/: <<Int>>/*class,abstract*/) = <<this>>/*keyword*/(<<"">>/*string*/)
        |  <<val>>/*keyword*/ <<a>>/*variable,readonly*/ = <<2>>/*number*/
        |  <<def>>/*keyword*/ <<a>>/*method*/(<<x>>/*parameter*/: <<Int>>/*class,abstract*/) = <<2>>/*number*/
        |  <<def>>/*keyword*/ <<a>>/*method*/(<<x>>/*parameter*/: <<Int>>/*class,abstract*/, <<y>>/*parameter*/: <<Int>>/*class,abstract*/) = <<2>>/*number*/
        |}""".stripMargin,
  )

  check(
    "locals",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<class>>/*keyword*/ <<Locals>>/*class*/ {
        |  {
        |    <<val>>/*keyword*/ <<x>>/*variable,readonly*/ = <<2>>/*number*/
        |    <<x>>/*variable,readonly*/ <<+>>/*method*/ <<2>>/*number*/
        |  }
        |}""".stripMargin,
  )
  check(
    "implicit-classes",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<object>>/*keyword*/ <<ImplicitClasses>>/*class*/ {
        |  <<implicit>>/*modifier*/ <<class>>/*keyword*/ <<Xtension>>/*class*/(<<number>>/*variable,readonly*/: <<Int>>/*class,abstract*/) {
        |    <<def>>/*keyword*/ <<increment>>/*method*/: <<Int>>/*class,abstract*/ = <<number>>/*variable,readonly*/ <<+>>/*method*/ <<1>>/*number*/
        |  }
        |  <<implicit>>/*modifier*/ <<class>>/*keyword*/ <<XtensionAnyVal>>/*class*/(<<private>>/*modifier*/ <<val>>/*keyword*/ <<number>>/*variable,readonly*/: <<Int>>/*class,abstract*/) <<extends>>/*keyword*/ <<AnyVal>>/*class,abstract*/ {
        |    <<def>>/*keyword*/ <<double>>/*method*/: <<Int>>/*class,abstract*/ = <<number>>/*variable,readonly*/ <<*>>/*method*/ <<2>>/*number*/
        |  }
        |}
        |""".stripMargin,
  )

  check(
    "for-comp",
    s"""|<<package>>/*keyword*/ <<example>>/*namespace*/
        |
        |<<class>>/*keyword*/ <<ForComprehensions>>/*class*/ {
        |  <<for>>/*keyword*/ {
        |    <<a>>/*parameter*/ <<<->>/*operator*/ <<List>>/*variable*/(<<1>>/*number*/)
        |    <<b>>/*parameter*/ <<<->>/*operator*/ <<List>>/*variable*/(<<a>>/*parameter*/)
        |    <<if>>/*keyword*/ (
        |      <<a>>/*parameter*/,
        |      <<b>>/*parameter*/
        |    ) <<==>>/*method*/ (<<1>>/*number*/, <<2>>/*number*/)
        |    (
        |      <<c>>/*variable,readonly*/,
        |      <<d>>/*variable,readonly*/
        |    ) <<<->>/*operator*/ <<List>>/*variable*/((<<a>>/*parameter*/, <<b>>/*parameter*/))
        |    <<if>>/*keyword*/ (
        |      <<a>>/*parameter*/,
        |      <<b>>/*parameter*/,
        |      <<c>>/*variable,readonly*/,
        |      <<d>>/*variable,readonly*/
        |    ) <<==>>/*method*/ (<<1>>/*number*/, <<2>>/*number*/, <<3>>/*number*/, <<4>>/*number*/)
        |    <<e>>/*variable,readonly*/ = (
        |      <<a>>/*parameter*/,
        |      <<b>>/*parameter*/,
        |      <<c>>/*variable,readonly*/,
        |      <<d>>/*variable,readonly*/
        |    )
        |    <<if>>/*keyword*/ <<e>>/*variable,readonly*/ <<==>>/*method*/ (<<1>>/*number*/, <<2>>/*number*/, <<3>>/*number*/, <<4>>/*number*/)
        |    <<f>>/*parameter*/ <<<->>/*operator*/ <<List>>/*variable*/(<<e>>/*variable,readonly*/)
        |  } <<yield>>/*keyword*/ {
        |    (
        |      <<a>>/*parameter*/,
        |      <<b>>/*parameter*/,
        |      <<c>>/*variable,readonly*/,
        |      <<d>>/*variable,readonly*/,
        |      <<e>>/*variable,readonly*/,
        |      <<f>>/*parameter*/
        |    )
        |  }
        |
        |}
        |""".stripMargin,
  )

}
