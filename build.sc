import mill._
import mill.scalalib._
import publish._
import ammonite.ops._

trait ScemplatePublishModule extends PublishModule {
  def artifactName = "scemplate"
  def publishVersion = "0.5.2"

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

object scemplate extends ScalaModule with ScemplatePublishModule {
  def scalaVersion = "2.13.1"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:2.3.0",
    ivy"com.lihaoyi::os-lib:0.7.0",
    ivy"com.propensive::magnolia:0.14.5",
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
      ivy"com.lihaoyi::utest::0.7.4"
    )
  }
}
