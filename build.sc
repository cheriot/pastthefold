import ammonite.ops._
import mill._
import mill.define.Target
import mill.scalalib._
import mill.util.Loose

object api extends SbtModule with GoogleAppEngine {
  def scalaVersion = "2.12.4"

  /**
    * Why is this not the same as?
    * mill api.runMain "news.pastthefold.graphql.MainRenderSchema" /Users/cheriot/devspace/storyline/api/src/main/resources/graphql.schema
    */
  def graphQLSchema() = T.command {
    val schemaPath: Path = millSourcePath / 'src / 'main / 'resources / "graphql.schema"
    runMain("news.pastthefold.graphql.MainRenderSchema", schemaPath.toString)
  }

  /** Since there are multiple Main classes. Specify the one to use with `mill api.run` */
  override def mainClass: T[Option[String]] = Some("news.pastthefold.Main")

  override def scalacPluginIvyDeps: Target[Loose.Agg[Dep]] = Agg(
    ivy"org.spire-math::kind-projector:0.9.7"
  )

  // Consider https://github.com/DavidGregory084/sbt-tpolecat/blob/master/src/main/scala/io/github/davidgregory084/TpolecatPlugin.scala
  // Steal the compiler flags list from better-files.
  override def scalacOptions = Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    // "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xfuture",                          // Turn on future language features.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  )

  // App Engine Standard needs a blocking servlet, which is not in the Http4s stable release. This snapshot is built
  // locally (ie it's not in remote repos).
  val http4sVersion = "0.18.16"
  val circeVersion = "0.9.3"
  val catsVersion = "1.0.1"
  val tsecVersion = "0.0.1-M11"
  override def ivyDeps = Agg(
    ivy"org.http4s::http4s-blaze-server:$http4sVersion",
    ivy"org.http4s::http4s-circe:$http4sVersion",
    ivy"org.http4s::http4s-dsl:$http4sVersion",
    ivy"org.http4s::http4s-servlet:$http4sVersion",
    ivy"org.http4s::http4s-testing:$http4sVersion",

    ivy"io.circe::circe-generic:$circeVersion",
    ivy"io.circe::circe-optics:$circeVersion",

    ivy"org.typelevel::cats-core:$catsVersion",
    ivy"org.typelevel::cats-effect:0.10.1",
    ivy"org.typelevel::kittens:1.1.0",

    ivy"com.typesafe:config:1.3.2",

    ivy"org.sangria-graphql::sangria:1.4.1",
    ivy"org.sangria-graphql::sangria-circe:1.2.1",

    ivy"io.github.jmcardon::tsec-http4s:$tsecVersion",

    ivy"com.lihaoyi::ammonite-ops:1.1.2",
    ivy"ch.qos.logback:logback-classic:1.2.3",
    ivy"com.google.appengine:appengine-api-1.0-sdk:1.9.53",
    // The most recent version supported by App Engine Standard.
    ivy"javax.servlet:javax.servlet-api:3.1.0"
  )

  object test extends Tests {
    override def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.3",
      ivy"io.circe::circe-parser:$circeVersion",
    )
    override def testFrameworks = Seq("utest.runner.Framework")
  }

  object it extends Tests {
    override def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.3",
      ivy"io.circe::circe-parser:$circeVersion",
    )
    override def intellijModulePath = millSourcePath / 'src / 'it
    override def sources = T.sources(
      millSourcePath / 'src / 'it / 'scala,
      millSourcePath / 'src / 'it / 'java
    )
    override def resources = T.sources{ millSourcePath / 'src / 'it / 'resources }
    override def testFrameworks = Seq("utest.runner.Framework")
  }
}

trait GoogleAppEngine extends JavaModule {
  def gaeWar = T {
    val deps: Agg[PathRef] = resolveDeps(T.task{ compileIvyDeps() ++ transitiveIvyDeps() })()
    GoogleAppEngine.build(
      T.ctx().dest,
      millSourcePath,
      compile().classes.path,
      deps
    )
  }

  // TODO def gaeDeploy
}

object GoogleAppEngine {

  def build(
             destPath: Path,
             sourcePath: Path,
             compiledClassesPath: Path,
             deps: Agg[PathRef]
           ): Unit = {

    def mkPath(path: Path): Path = {
      mkdir(path)
      path
    }

    val exploded = mkPath(destPath / "exploded")
    val webInf = mkPath(exploded / "WEB-INF")

    // exploded/WEB-INF/web.xml
    //                 /appengine-web.xml
    //                 /classes/**/**.class
    cp.into(sourcePath / "web.xml", webInf)
    cp.into(sourcePath / "appengine-web.xml", webInf)
    cp(compiledClassesPath, webInf / "classes")

    // exploded/WEB-INF/lib/*.jar
    var libPath = mkPath(webInf / "lib")
    deps.map(pathRef =>
      cp.into(pathRef.path, libPath)
    )

    // Static assets will be served from elsewhere, but could go in exploded/
  }
}
