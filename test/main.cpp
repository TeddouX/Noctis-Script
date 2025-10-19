#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <ncsc/compiler.hpp>
#include <ncsc/vm.hpp>
#include <print>
#include <fstream>
#include <chrono>

int main() {
    std::string code = 
    // "Int add(Int i, Int i1) {\n"
    // "   return i+i1;\n"
    // "}\n"

    // "Int globalVar = add(1, 1);\n"
    // "Int globalVar1 = globalVar + 4;\n"

    "fun main() {\n"
    // "   Int16 a = 1;\n"
    // "   UInt16 b = 1;\n"
    "   Bool c = false;\n"
    "   Bool c = true;\n"
    "   Bool f = 1;\n"
    "   Bool g = 0;\n"
    "}\n";

    // "fun addNoRet(Int i, Int i1) {\n"
    // "   Int a = i+i1;\n"
    // "}\n"
    // "Int add(Int i, Int i1) {\n"
    // "   return i+i1;\n"
    // "}\n"
    // "Int addMultiple(Int i, Int i1, Int i2) {\n"
    // "   return i+i1+i2;\n"
    // "}\n"
    // "Int mul(Int i, Int i1) {\n"
    // "   return i*i1;\n"
    // "}\n"
    // "fun main() {\n"
    // "   addNoRet(1,1);\n"
    // "   Int a = add(1,2);\n"
    // "   Int b = addMultiple(3,4,5);\n"
    // "   Int c = mul(6,7);\n"
    // "   Int d = mul(2,2)*mul(2,2);\n"
    // "}\n";
    auto tokens = NCSC::Lexer(code).tokenizeAll();

    std::println("{}", code);

    for (auto token : tokens) {
        std::println("{} Position: {}:{}", token.getStrRepr(), token.line, token.col);
    }
    std::println();

    NCSC::Parser parser(tokens);
    auto rootNode = parser.parseAll();
    std::println("{}", rootNode.getStrRepr());
    if (parser.hasErrors()) {
        for (auto error : parser.getErrors()) 
            std::println("{}", error.getString());

        exit(EXIT_FAILURE);
    }

    NCSC::Compiler compiler;
    std::shared_ptr<NCSC::Script> script = compiler.compileScript(rootNode);
    if (compiler.hasErrors()) {
        for (auto error : compiler.getErrors())
            std::println("{}", error.getString());

        exit(EXIT_FAILURE);
    }

    for (const auto &var : script->getAllGlobalVars())
        std::println("global {}:\n{}", var.name, NCSC::Compiler::disassemble(var.bytecode));
    
    for (const auto &fun : script->getAllFunctions())
        std::println("fun {}:\n{}", fun.name, NCSC::Compiler::disassemble(fun.bytecode));

    const NCSC::Function *fun = script->getFunction("main");
    if (fun) {
        NCSC::VM vm(script);
        vm.computeGlobals();
        vm.prepareFunction(fun);
        
        bool success = vm.execute();
        if (!success)
            std::println("Error: {}", vm.getLastError());
        else
            std::println("Stack: {}", vm.getStackStrRepr());
    }

    return 0;
}
