To only test Scala 2 tests, set the `CORE_TESTS_ONLY` environment variable to `true`.

And then run:

`sbt 'unit/testOnly; ++2.12.17; cross/test'`