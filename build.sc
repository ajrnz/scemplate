import mill._
import mill.scalalib._
import publish._
import ammonite.ops._

trait ScemplatePlublishModule extends PublishModule {
  def artifactName = "scemplate"
  def publishVersion = "0.5.1-SNAPSHOT"

  def pomSettings = PomSettings(
    description = artifactName() + " - scala template engine and simple expression evaluator",
    organization = "com.github.ajrnz",
    url = "https://github.com/ajrnz/scemplate",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("ajrnz", "scemplate"),
    developers = Seq(
      Developer("ajrnz", "Andrew Richards", "https://github.com/ajrnz")
    )
  )
}

object scemplate extends ScalaModule with ScemplatePlublishModule {
  def scalaVersion = "2.12.8"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:2.0.5",
    ivy"com.lihaoyi::os-lib:0.2.6",
    ivy"com.propensive::magnolia:0.7.1",
  )

  object test extends Tests {
    def testFrameworks = Seq("utest.runner.Framework")

    def test(args: String*) = T.command {
      jar() // make a jar of the test resources for use during the tests
      super.test(args: _*)()
    }

    def testLocal(args: String*) = T.command {
      jar() // see above
      super.testLocal(args: _*)()
    }

    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.6.4"
    )
  }
}
