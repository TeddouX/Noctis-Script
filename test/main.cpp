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

    // "fun main() {\n"
    // "   UInt64 a = 1;\n"
    // "   UInt64 b = 1;\n"
    // "   UInt64 c = a + b;\n"
    // "}\n";

    "fun addNoRet(Int i, Int i1) {\n"
    "   Int a = i+i1;\n"
    "}\n"
    "Int add(Int i, Int i1) {\n"
    "   return i+i1;\n"
    "}\n"
    "Int addMultiple(Int i, Int i1, Int i2) {\n"
    "   return i+i1+i2;\n"
    "}\n"
    "Int mul(Int i, Int i1) {\n"
    "   return i*i1;\n"
    "}\n"
    "fun main() {\n"
    "   addNoRet(1,1);\n"
    "   Int a = add(1,2);\n"
    "   Int b = addMultiple(3,4,5);\n"
    "   Int c = mul(6,7);\n"
    "   Int d = mul(2,2)*mul(2,2);\n"
    "}\n";
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

// #include "ncsc/ncsc.hpp"
// #include <print>

// size_t stackSize = 4*(sizeof(int64_t) + sizeof(NCSC::DWord));
// auto stack = static_cast<NCSC::Byte *>(std::calloc(stackSize, sizeof(NCSC::Byte)));
// size_t sp = stackSize;

// void pop() {
//     assert(sp > 0 && "Stack underflow");

//     size_t popSize = 0;
//     NCSC::Value val = NCSC::readValue(stack, sp, popSize);
//     sp -= popSize;

//     std::println("Val: {}", val.i16);

//     // Fill popped value with zeroes
//     std::memset(stack + sp, 0, popSize);
// }

// void print() {
//     std::print("0x");
//     for (size_t i = 0; i < stackSize; i++)
//         std::print("{:X}", (NCSC::Byte)stack[i]);
//     std::println();
// }

// int main(void) {
//     for (int i = 0; i < 4; i++) {
//         int64_t val = -i-1;
//         NCSC::Byte intBytes[NCSC::getValueSize(val)]{0};
//         NCSC::makeValue(val, NCSC::ValueType::INT64, intBytes, 0);

//         std::memcpy(stack + i * NCSC::getValueSize(val), intBytes, sizeof(intBytes));
//     }

//     free(stack);

//     return 0;
// }
