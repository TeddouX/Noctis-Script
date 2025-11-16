// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <gtest/gtest.h>
#include <ncsc/compiler.hpp>
#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>

#include <fstream>
#include <expected>

using namespace NCSC;

const std::string SCRIPTS_FOLDER_PATH = "noctis_script/tests/scripts/";
const auto scriptCtx = ScriptContext::create();

enum class CheckErrorsStatus {
    OK,
    FAILED_TO_OPEN,
    HAS_ERROR_IN_CHECK_SUCCESS,  
    ERROR_IS_NOT_THROWN,
    SYNTAX_ERROR,
};

using CheckErrorsRes = std::pair<CheckErrorsStatus, std::string>;
static CheckErrorsRes checkErrors(const std::string &name) {

    // ---- Read file and compile the script
    std::ifstream ifs;

    std::stringstream buf;
    std::filesystem::path filePath = std::filesystem::absolute(SCRIPTS_FOLDER_PATH + name + ".ncsc");
    ifs.open(filePath);

    if (!ifs.is_open())
        return { CheckErrorsStatus::FAILED_TO_OPEN, filePath.string() };

    buf << ifs.rdbuf();

    std::string fileContents = buf.str();

    auto src = ScriptSource::fromSource(fileContents);
    src->filePath = filePath;

    auto tokens = Lexer(src).tokenizeAll();
    Parser parser(tokens, src);
    ASTNode rootNode = parser.parseAll();
    if (parser.hasErrors())
        return { 
            CheckErrorsStatus::SYNTAX_ERROR, 
            parser.getErrors()[0].getErrorMessage(true) 
        };

    Compiler compiler(scriptCtx, false);
    compiler.compileScript(src, rootNode);
    const std::vector<Error> &errors = compiler.getErrors();


    // ---- Check if the errors are being thrown 
    // ---- in the right locations
    
    // Line, col
    using Location = std::pair<size_t, size_t>;
    // Def start, Def end
    using DefinitionRange = std::pair<Location, Location>;
    
    std::unordered_map<const std::string *, DefinitionRange> functionsDefs;
    
    for (const auto &child : rootNode.children()) {
        if (child.type() == ASTNodeType::FUNCTION) {
            Location start(child.location.line, child.location.col);
            Location end(child.location.lineEnd, child.location.colEnd);

            const std::string &funcName = child.child(1).token().val;

            functionsDefs.emplace(&funcName, DefinitionRange{start, end});
        }
    }

    auto isErrorInDefRange = [errors](DefinitionRange defRange, 
                            const std::string &errPref = "", uint32_t num = 0) -> const Error * {
        for (const auto &err : errors) {
            const NCSC::Location &errLoc = err.getLocation();

            // Is the error on the same line as the definition's start?
            bool errAfterRangeBegin = errLoc.line == defRange.first.first 
                // If yes, compare columns
                ? errLoc.col > defRange.first.second
                // Else, compare lines 
                : errLoc.line > defRange.first.first;

            bool errBeforeRangeEnd = errLoc.line == defRange.second.first 
                ? errLoc.col < defRange.second.second
                : errLoc.line < defRange.second.first;

            if (errAfterRangeBegin && errBeforeRangeEnd) {
                // The caller expects and error        
                if (!errPref.empty()) {
                    if (err.getInfo().numPrefix == errPref && err.getInfo().num == num)
                        return &err;
                    continue;
                }

                // Else the caller is just checking for any error
                return &err;
            }
        }

        return nullptr;
    };

    for (const auto &[funcName, defRange] : functionsDefs) {
        if (*funcName == "TestSuccess") {
            if (auto err = isErrorInDefRange(defRange))
                return { 
                    CheckErrorsStatus::HAS_ERROR_IN_CHECK_SUCCESS, 
                    err->getErrorMessage() 
                };
            
            continue;
        }

        if (!funcName->starts_with("TestFail")) 
            continue;

        std::string funSuffix = funcName->substr(sizeof("TestFail" - 1));
        std::string errPref;
        std::string errNumStr;

        for (char c : funSuffix) {
            if (isalpha(c))
                errPref += c;
            else if (isdigit(c))
                errNumStr += c;
            else
                break;
        }

        uint32_t errNum = strtoul(errNumStr.c_str(), nullptr, 0);
        if (!isErrorInDefRange(defRange, errPref, errNum))
            return {
                CheckErrorsStatus::ERROR_IS_NOT_THROWN, 
                std::format("Compilation error {}{} isn't throw in '{}'.", errPref, errNum, *funcName) 
            };
    }

    return { CheckErrorsStatus::OK, "" };
}

static ::testing::AssertionResult CheckErrRes(const char *expr, const CheckErrorsRes &checkErrRes) {
    switch (checkErrRes.first) {
        case CheckErrorsStatus::OK: 
            return ::testing::AssertionSuccess();
        case CheckErrorsStatus::FAILED_TO_OPEN:
            return ::testing::AssertionFailure() << std::format("{} failed. Failed to open file: {}", expr, checkErrRes.second);
        case CheckErrorsStatus::HAS_ERROR_IN_CHECK_SUCCESS: 
            return ::testing::AssertionFailure() << std::format("{} failed. Has error in check success: {}", expr, checkErrRes.second);
        case CheckErrorsStatus::ERROR_IS_NOT_THROWN:
            return ::testing::AssertionFailure() << std::format("{} failed. {}", expr, checkErrRes.second);
        case CheckErrorsStatus::SYNTAX_ERROR:
            return ::testing::AssertionFailure() << std::format("{} failed. Syntax error: {}", expr, checkErrRes.second);

        default: return ::testing::AssertionFailure() << "Unhandled 'CheckErrorsStatus' enum literal.";
    }
}

#define ASSERT_CHECK_ERR_RES_OK(filename) EXPECT_PRED_FORMAT1(CheckErrRes, checkErrors(filename))

TEST(CompilerTests, Variables) {
    ASSERT_CHECK_ERR_RES_OK("variables_compiler");
}

TEST(CompilerTests, Functions) {
    ASSERT_CHECK_ERR_RES_OK("functions_compiler");
}

TEST(CompilerTests, IncsDecs) {
    ASSERT_CHECK_ERR_RES_OK("incs_decs");
}
