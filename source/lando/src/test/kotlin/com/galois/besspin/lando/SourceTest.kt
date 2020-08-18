package com.galois.besspin.lando

import com.galois.besspin.lando.ssl.ast.RawSSL
import com.galois.besspin.lando.ssl.ast.rawSSLFromJSON
import com.galois.besspin.lando.ssl.parser.parseFile
import kotlinx.serialization.ImplicitReflectionSerializer
import org.junit.ComparisonFailure
import org.junit.Rule
import org.junit.Test
import org.junit.rules.ExpectedException
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import java.io.File
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

sealed class SourceTestType {
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

//
//Tests parsing of all .lando source files in the test/lando directory,
//in the style of the Glasgow Haskell Compiler's (GHC) test framework.
//
@RunWith(Parameterized::class)
class SourceTest(
    private val source: File,
    private val sourceString: String,
    private val testType: SourceTestType
) {
    companion object {
        @JvmStatic
        @Parameterized.Parameters(name="{1}")
        fun tests() : Iterable<Array<Any>> {
            val testTypesToRun: List<SourceTestType> =
                listOf(SourceTestType.ShouldFail, SourceTestType.ShouldParse, SourceTestType.ShouldWarn)

            val toTest: MutableList<Array<Any>> = mutableListOf()
            for (topDir in File("src/test/lando/").listFiles()!!) {
                for (testType in testTypesToRun) {
                    for (file in File(topDir, testType.dirName()).walkTopDown()) {
                        if (file.extension == "lando")
                            toTest.add(arrayOf(file, file.path.removePrefix("src/test/lando/"), testType))
                    }
                }
            }
            return toTest
        }
    }

    @Test
    @ImplicitReflectionSerializer
    fun testFile() {
        assertEquals("lando", source.extension, "is not a lando file")

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
                assertNotNull(errorMsg, "did not generate any errors")
                assertTrue(errFile.exists(), "does not exist")
                assertEquals(errFile.readText().trim(), errorMsg, "error mismatch:\n")
            }
            is SourceTestType.ShouldWarn -> {
                assertNotNull(warnMsg, "did not generate any warnings")
                assertTrue(warnFile.exists(), "does not exist")
                assertEquals(warnFile.readText().trim(), warnMsg, "warnings mismatch:\n")
                //the below is identical to that in the ShouldParse case
                assertNotNull(ast, "did not parse")
                assertTrue(jsonFile.exists(), "does not exist")
                assertEquals(rawSSLFromJSON(jsonFile.readText().trim()), ast, "does not match JSON:\n")
            }
            is SourceTestType.ShouldParse -> {
                assertNotNull(ast, "did not parse")
                assertTrue(jsonFile.exists(), "does not exist")
                assertEquals(rawSSLFromJSON(jsonFile.readText().trim()), ast, "does not math JSON:\n")
            }
        }
    }
}
