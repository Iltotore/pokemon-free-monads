import mill._
import mill.scalalib.scalafmt.ScalafmtModule
import scalalib._

object main extends RootModule with ScalaModule with ScalafmtModule {

  def scalaVersion = "3.3.1"

  object test extends ScalaTests {

    def testFramework = "utest.runner.Framework"

    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.8.1"
    )
  }
}