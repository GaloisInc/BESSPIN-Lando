package com.galois.besspin.lando

import com.galois.besspin.lando.ssl.ast.RawSSL
import com.galois.besspin.lando.ssl.ast.rawSSLFromJSON
import com.galois.besspin.lando.ssl.parser.parseFile
import kotlinx.serialization.ImplicitReflectionSerializer
import org.junit.Test
import java.io.File
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

//
//Tests parsing of all .lando source files in the test/lando directory,
//in the style of the Glasgow Haskell Compiler's (GHC) test framework.
//
class SourceTest {

    private sealed class SourceTestType {
        object ShouldFail:  SourceTestType()
        object ShouldParse: SourceTestType()
        object ShouldWarn:  SourceTestType()

        fun dirName(): String =
            when (this) {
                is ShouldFail  -> "should_fail"
                is ShouldParse -> "should_parse"
                is ShouldWarn  -> "should_warn"
            }
    }
    private val testTypesToRun: List<SourceTestType> =
        listOf(SourceTestType.ShouldFail, SourceTestType.ShouldParse, SourceTestType.ShouldWarn)

    @Test
    @ImplicitReflectionSerializer
    fun testAll() {
        val toTest: MutableList<Pair<File, SourceTestType>> = mutableListOf()
        for (topDir in File("src/test/lando/").listFiles()!!) {
            for (testType in testTypesToRun) {
                for (file in File(topDir, testType.dirName()).walkTopDown()) {
                    if (file.extension == "lando")
                        toTest.add(Pair(file, testType))
                }
            }
        }

        println("Going to test ${toTest.size} lando source files.")
        for ((file, testType) in toTest) {
            println(file.path)
            testFile(file, testType)
        }
    }

    @ImplicitReflectionSerializer
    private fun testFile(source: File, testType: SourceTestType) {
        assertEquals("lando", source.extension, "${source.path} is not a lando file")

        var errorMsg: String? = null; var warnMsg: String? = null; var ast: RawSSL? = null
        try {
            val (astOut, warnMsgOut) = parseFile(source)
            warnMsg = warnMsgOut.trim(); ast = astOut
        } catch (ex: Exception) {
            errorMsg = ex.message?.trim()
        }

        val filename = source.nameWithoutExtension
        val errFile  = File(source.parentFile, "$filename.errors")
        val warnFile = File(source.parentFile, "$filename.warnings")
        val jsonFile = File(source.parentFile, "$filename.json")

        when (testType) {
            is SourceTestType.ShouldFail -> {
                assertNotNull(errorMsg, "${source.path} did not generate any errors")
                assertTrue(errFile.exists(), "${errFile.path} does not exist")
                assertEquals(errFile.readText().trim(), errorMsg, "${source.path} error mismatch:\n")
            }
            is SourceTestType.ShouldWarn -> {
                assertNotNull(warnMsg, "${source.path} did not generate any warnings")
                assertTrue(warnFile.exists(), "${warnFile.path} does not exist")
                assertEquals(warnFile.readText().trim(), warnMsg, "${source.path} warnings mismatch:\n")
                //the below is identical to that in the ShouldParse case
                assertNotNull(ast, "${source.path} did not parse")
                assertTrue(jsonFile.exists(), "${jsonFile.path} does not exist")
                assertEquals(rawSSLFromJSON(jsonFile.readText().trim()), ast,
                             "${source.path} does not math ${jsonFile.path}:\n")
            }
            is SourceTestType.ShouldParse -> {
                assertNotNull(ast, "${source.path} did not parse")
                assertTrue(jsonFile.exists(), "${jsonFile.path} does not exist")
                assertEquals(rawSSLFromJSON(jsonFile.readText().trim()), ast,
                    "${source.path} does not math ${jsonFile.path}:\n")
            }
        }
    }
}
