scalaVersion := "2.12.6"

resolvers += Resolver.sonatypeRepo("staging")

libraryDependencies += "com.bot4s" %% "telegram-core" % "4.0.0-RC1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"