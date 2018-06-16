package ajr.scemplate

import ammonite.ops._
import utest._
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader

object TestDirectoryRenderer extends TestHelper {
  val allDir = pwd / 'scemplate / 'test / 'resources / 'templates / 'all
  val outDir = pwd / 'out / 'testDirRender
  rm.!(outDir)

  val tests = Tests {
    'allTemplate - {
      def checkContent(out: Path, shouldHaveReadme: Boolean = true) = {
        haveDir(out / "1")
        haveFile(out / "1" / "THIS.txt", "This is: THIS\n")
        haveFile(out / "1" / "2"/ "that.txt", "I am 21\n")
        if (shouldHaveReadme)
          haveFile(out / "Readme.txt", "Do not distribute\n" )
        else
          noFile(out / "Readme.txt")
      }

      'basic - {
        'resources - {
          val reader = DirectoryRenderer.resourceReader("templates/all")
          val out = outDir / 'allResources
          val writer = DirectoryRenderer.fileSystemWriter(out)

          DirectoryRenderer.renderTree(testContext, reader, writer)
          checkContent(out)
        }

        'files - {
          val reader = DirectoryRenderer.fileSystemReader(allDir)
          val out = outDir / 'allFiles
          val writer = DirectoryRenderer.fileSystemWriter(out)

          DirectoryRenderer.renderTree(testContext, reader, writer)
          checkContent(out)
        }

        'jar - {
          val reader = jarReader(_ => false)
          val out = outDir / 'allJar
          val writer = DirectoryRenderer.fileSystemWriter(out)
          DirectoryRenderer.renderTree(testContext, reader, writer)
          checkContent(out)
        }
      }

      'ignore  -{
        'resources - {
          val reader = DirectoryRenderer.resourceReader("templates/all", ignorePath = _ == RelPath("Readme.txt"))
          val out = outDir / 'allIgnoreResources
          val writer = DirectoryRenderer.fileSystemWriter(out)
          DirectoryRenderer.renderTree(testContext, reader, writer)
          checkContent(out, shouldHaveReadme = false)
        }

        'files - {
          val reader = DirectoryRenderer.fileSystemReader(allDir, ignorePath = _ == RelPath("Readme.txt"))
          val out = outDir / 'allIgnoreFiles
          val writer = DirectoryRenderer.fileSystemWriter(out)
          DirectoryRenderer.renderTree(testContext, reader, writer)
          checkContent(out, shouldHaveReadme = false)
        }

        'jar - {
          val reader = jarReader(ignorePath = _ == RelPath("Readme.txt"))
          val out = outDir / 'allIgnoreJar
          val writer = DirectoryRenderer.fileSystemWriter(out)
          DirectoryRenderer.renderTree(testContext, reader, writer)
          checkContent(out, shouldHaveReadme = false)
        }
      }

      'isTemplate - {
        val reader = DirectoryRenderer.resourceReader("templates/all")
        val out = outDir / 'isTemplate
        val writer = DirectoryRenderer.fileSystemWriter(out)
        DirectoryRenderer.renderTree(testContext, reader, writer, isTemplate = _.segments.last != "that.txt")
        haveFile(out / "1" / "2"/ "that.txt", "I am $twenty_one\n")
      }

      'renamePath - {
        val reader = DirectoryRenderer.resourceReader("templates/all")
        val out = outDir / 'renamePath
        val writer = DirectoryRenderer.fileSystemWriter(out)
        val renamePath = { p: RelPath =>
          if (p.segments.last == "that.txt")
            p / up / up / "moved.txt"
          else p
        }
        DirectoryRenderer.renderTree(testContext, reader, writer, renamePath = renamePath)
        haveFile(out / "1" / "moved.txt", "I am 21\n")
      }

      'badRename - intercept[BadPathException] {
        val reader = DirectoryRenderer.resourceReader("templates/all")
        val out = outDir / 'badRename
        val writer = DirectoryRenderer.fileSystemWriter(out)
        val renamePath = { p: RelPath =>
          if (p.segments.last == "that.txt")
            p / up / up / up / up / "moved.txt"
          else p
        }
        DirectoryRenderer.renderTree(testContext, reader, writer, renamePath = renamePath)
      }
      'badName - intercept[BadPathException] {
        import implicits._
        val reader = DirectoryRenderer.resourceReader("templates/bad1")
        val out = outDir / 'badName
        val writer = DirectoryRenderer.fileSystemWriter(out)
        val newContext = Context().withValues("upUpAndAway" -> "../../../etc/passwd")
        DirectoryRenderer.renderTree(newContext, reader, writer)
      }
    }
  }

  private def jarReader(ignorePath: RelPath => Boolean) = {
    val url = (pwd / RelPath("out/scemplate/test/jar/dest/out.jar")).toIO.toURI.toURL
    val cl = new URLClassLoader(Array(url), null)
    val clazz = Class.forName("ajr.scemplate.Dummy", true, cl)
    DirectoryRenderer.resourceReader("templates/all", ignorePath, clazz)
  }

  def haveDir(dir: Path) = {
    assert(dir.isDir)
  }

  def noFile(dir: Path) = {
    assert(!dir.toIO.exists)
  }

  def haveFile(file: Path, expectedContent: String) = {
    assert(file.isFile)
    val content = read(file)
    content ==> expectedContent
  }

}
