package ajr.scemplate

import java.io.File
import java.util.jar.JarFile
import scala.collection.JavaConverters._
import ammonite.ops._
import geny.Generator
import ajr.scemplate.Template.render


object DirectoryRenderer {
  type DirectoryReader = Generator[(RelPath, Option[String])]
  type DirectoryWriter = (RelPath, Option[String]) => Unit

  def fileSystemReader(basePath: Path, ignorePath: RelPath => Boolean = _ => false): DirectoryReader = {
    Generator.from(ls.rec.!(basePath).listed.filterNot(ignorePath))
      .map{ relpath =>
        val f = (basePath / relpath)
        if (f.isFile)
          relpath -> Some(read(f))
        else
          relpath -> None
      }
  }

  def fileSystemWriter(basePath: Path, overwrite: Boolean = false): DirectoryWriter =
    (path, itemContent) => {
      checkPath(path)

      val fullPath = basePath / path
      itemContent match {
        case None =>
          if (!fullPath.toIO.exists)
            mkdir(fullPath)

        case Some(content) =>
          write(fullPath, content)
      }
    }

  def resourceReader(basePath: String, ignorePath: RelPath => Boolean = _ => false, clazz: Class[_] = getClass): DirectoryReader = {
    val jarFile = new File(clazz.getProtectionDomain.getCodeSource.getLocation.getPath)
    val basePathSlash = if (basePath.endsWith("/")) basePath else basePath + "/"
    val basePathLen = basePathSlash.length

    if (jarFile.isFile) { // JAR file in classpath - open it and read entries
      val jar = new JarFile(jarFile)
      Generator.selfClosing(jar.entries.asScala, () => jar.close())
        .filter{x =>
          x.getName.startsWith(basePath) &&
          x.getName.length > basePathLen && // ignore the top resource (jar) 'directory' itself
          !ignorePath(RelPath(x.getName.substring(basePathLen)))
        }
        .map { e =>
          if (e.getName.endsWith("/"))
            RelPath(e.getName.substring(basePathLen)) -> None
          else {
            val is = jar.getInputStream(e)
            val data = read(is)
            is.close()
            RelPath(e.getName.substring(basePathLen)) -> Some(data)
          }
        }
    }
    else { // in local file system
      val url = getClass.getResource("/" + basePath)
      if (url == null)
        throw new IllegalArgumentException(s"Can't access resource at path: $basePath")
      fileSystemReader(Path(new File(url.toURI)), ignorePath)
    }
  }

  def renderTree(context: Context,
                 reader: DirectoryReader, writer: DirectoryWriter,
                 isTemplate: RelPath => Boolean = _ => true,
                 renamePath: RelPath => RelPath = identity) =
  {
    reader.foreach { case (path, content) =>
      val dstPath = renamePath(RelPath(render(path.toString)(context)))
      checkPath(dstPath)

      val renderedContent = content.map { src =>
        if (isTemplate(path))
          render(src)(context)
        else src
      }
      writer(dstPath, renderedContent)
    }
  }

  private def checkPath(path: RelPath) = {
    if (path.ups != 0)
      throw new BadPathException(s"Paths must not escape base directory: ($path)")
  }
}
