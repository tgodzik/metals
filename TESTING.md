To only test Scala 2 tests:
1. Set the `CORE_TESTS_ONLY` environment variable to `true`.

2. Set COURSIER_CACHE to save all the downloaded artifacts locally:

```
export COURSIER_CACHE=$(pwd)/metals-cache/coursier
```

And then run:

`sbt 'unit/testOnly; ++2.12.17; cross/test'`

3. Pack up the cache and and you can rerun with the same PATHS without access to internet
