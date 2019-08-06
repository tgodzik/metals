package bench
import io.methvin.watcher.DirectoryWatcher
import java.nio.file.Path
import io.methvin.watcher.DirectoryChangeListener
import io.methvin.watcher.DirectoryChangeEvent
import java.nio.file.Files
import java.nio.file.Paths
object MainBench {
  def main(args: Array[String]): Unit = {
    // val bench = new ClasspathSymbolsBench()
    // bench.setup()
    // bench.run()
    // bench.run()

    import scala.collection.JavaConverters._
    pprint.log("before")
    startWatching(List(Paths.get("/home/tgodzik/Documents/test/")).asJava)
    pprint.log("after")
  }

  private def startWatching(paths: java.util.List[Path]): Unit = {
    val watcher = DirectoryWatcher
      .builder()
      .paths(paths)
      .listener(new Listener)
      .build()
    watcher.watch()
  }

  class Listener extends DirectoryChangeListener {
    override def onEvent(event: DirectoryChangeEvent): Unit = {
      pprint.log(event)
    }
  }
}
