/*
 * Copyright 2011-2012 Typesafe Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.typesafe.sbt

import sbt.Keys._
import sbt.{ File, FileFilter, _ }

import scala.collection.immutable.Seq
import scalariform.formatter.ScalaFormatter
import scalariform.formatter.preferences.IFormattingPreferences
import scalariform.utils.TextEdit

sealed trait FormatResult
case object FormattedCorrectly extends FormatResult
case object NotFormattedCorrectly extends FormatResult

import com.typesafe.sbt.Scalariform._

import scala.io.Source

private object ScalariformTest {

  def apply(
    preferences:       IFormattingPreferences,
    sourceDirectories: Seq[File],
    includeFilter:     FileFilter,
    excludeFilter:     FileFilter,
    ref:               ProjectRef,
    configuration:     Configuration,
    streams:           TaskStreams,
    scalaVersion:      String
  ): Seq[File] = {

    def log(label: String, logger: Logger)(message: String)(count: String) = {
      logger.error(message.format(count, label))
    }

    def checkSource(source: Source, doFormat: String ⇒ List[TextEdit]): FormatResult = {
      val original = source.mkString
      doFormat(original) match {
        case Nil => FormattedCorrectly
        case _   => NotFormattedCorrectly
      }
    }

    def testFormat(files: Set[File], logFun: String => Unit): Boolean = {
      (for (file <- files if file.exists) yield {
        val doFormat = ScalaFormatter.formatAsEdits(_: String, preferences, scalaVersion = pureScalaVersion(scalaVersion))
        val contents = Source.fromFile(file)
        checkSource(contents, doFormat) match {
          case FormattedCorrectly => true
          case NotFormattedCorrectly => {
            logFun(s"File not properly formatted: $file")
            false
          }
        }
      }).fold(true)(_ && _)
    }

    val files = sourceDirectories.descendantsExcept(includeFilter, excludeFilter).get.toSet
    val cache = streams.cacheDirectory / "scalariform"
    val logFun = log("%s(%s)".format(Reference.display(ref), configuration), streams.log) _
    if (preferencesChanged(streams.cacheDirectory / "scalariform-preferences")(preferences)) {
      IO.delete(cache)
    }
    handleFiles(files, cache, logFun("Checking format: %s %s ..."), testFormat).toList
  }

  def handleFiles(
    files:     Set[File],
    cache:     File,
    logFun:    String => Unit,
    updateFun: (Set[File], String => Unit) => Boolean
  ): Set[File] = {

    def handleUpdate(in: ChangeReport[File], out: ChangeReport[File]) = {
      val files = in.modified -- in.removed
      inc.Analysis.counted("Scala source", "", "s", files.size) foreach logFun
      if (!updateFun(files, logFun)) {
        sys.error("Validation failed")
      }
      files
    }

    FileFunction.cached(cache)(FilesInfo.hash, FilesInfo.exists)(handleUpdate)(files)
  }
}
