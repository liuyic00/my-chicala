#!/usr/bin/env -S scala-cli shebang
//> using repository jitpack
//> using dep com.github.kzns:srun:65eaf75

import srun.tool.*

Bash("make test-debug").run()
Bash("scalafmt test_run_dir/chiselToScala/test").call(
  stdin = os.Inherit,
  stdout = os.Inherit,
  stderr = os.Inherit,
  check = false
)

filesUnder(os.pwd / "test_run_dir/chiselToScala/test", "scala")
  .filter(_.toString.endsWith("sorted.scala"))
  // .dropWhile({ testFile =>
  //  val lastFile = os.pwd / "test_run_dir/chiselToScala/testlast" /
  //    testFile.relativeTo(os.pwd / "test_run_dir/chiselToScala/test")
  //  os.read(testFile) == os.read(lastFile)
  // })
  // .headOption
  .foreach { testFile =>
    val lastFile = os.pwd / "test_run_dir/chiselToScala/testlast" /
      testFile.relativeTo(os.pwd / "test_run_dir/chiselToScala/test")
      if os.read(testFile) != os.read(lastFile) then
        Bash(s"code --diff \"$lastFile\" \"$testFile\"").spawn()
  }
