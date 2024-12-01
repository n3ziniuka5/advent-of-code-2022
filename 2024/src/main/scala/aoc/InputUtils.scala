package aoc

import java.io.File
import java.nio.file.Files

object InputUtils:
    val cacheDir = ".cache"
    val session  = sys.env("AOC_SESSION")

    // sample numbers start from 1, don't judge me
    def fetchSample(year: Int, day: Int, sampleNumber: Int = 1): List[String] =
        val samplesFolder = s"$cacheDir/$year/$day"
        File(samplesFolder).mkdirs()

        val samplePath = s"$samplesFolder/sample-$sampleNumber.txt"
        val sampleFile = File(samplePath)

        if sampleFile.exists() then io.Source.fromFile(sampleFile).getLines().toList
        else
            val response = requests
                .get(s"https://adventofcode.com/$year/day/$day", headers = "cookie" -> s"session=$session" :: Nil)
                .text()

            def extractSample(currentNumber: Int, searchStart: Int): String =
                val startToken  = "<pre><code>"
                val endToken    = "</code></pre>"
                val sampleStart = response.indexOf(startToken, searchStart)
                if sampleStart == -1 then throw new RuntimeException(s"Sample $currentNumber not found")
                else if currentNumber == sampleNumber then
                    val sampleEnd = response.indexOf(endToken, sampleStart)
                    response.slice(sampleStart + startToken.length, sampleEnd)
                else extractSample(currentNumber + 1, sampleStart + startToken.length)

            val sample = extractSample(1, 0)

            Files.write(sampleFile.toPath, sample.getBytes())

            sample.split("\n").toList

    def fetchInput(year: Int, day: Int): List[String] =
        val inputFolder = s"$cacheDir/$year/$day"
        File(inputFolder).mkdirs()

        val inputFile = File(s"$inputFolder/input.txt")

        if inputFile.exists() then io.Source.fromFile(inputFile).getLines().toList
        else
            val response = requests
                .get(s"https://adventofcode.com/$year/day/$day/input", headers = "cookie" -> s"session=$session" :: Nil)
                .text()

            Files.write(inputFile.toPath, response.getBytes())

            response.split("\n").toList
