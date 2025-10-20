#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <ncsc/compiler.hpp>
#include <ncsc/vm.hpp>
#include <print>
#include <fstream>
#include <chrono>

int main() {
    std::ifstream ifs;

    std::stringstream buf;
    ifs.open("test/code.ncsc");
    buf << ifs.rdbuf();

    std::string fileContents = buf.str();

    // auto tokens = NCSC::Lexer(fileContents).tokenizeAll();
    // for (auto token : tokens) {
    //     std::println("{} Position: {}:{}", token.getStrRepr(), token.line, token.col);
    // }
    // std::println();

    // NCSC::Parser parser(tokens);
    // auto rootNode = parser.parseAll();
    // std::println("{}", rootNode.getStrRepr());
    // if (parser.hasErrors()) {
    //     for (auto error : parser.getErrors()) 
    //         std::println("{}", error.getString());

    //     exit(EXIT_FAILURE);
    // }

    std::println("{}", fileContents);

    NCSC::Compiler compiler;
    std::shared_ptr<NCSC::Script> script = compiler.compileScript(fileContents);
    if (compiler.hasErrors()) {
        for (auto error : compiler.getErrors())
            std::println("{}", error.getString());

        exit(EXIT_FAILURE);
    }

    for (const auto &var : script->getAllGlobalVars())
        std::println("global {}:\n{}", var.name, NCSC::Compiler::disassemble(var.bytecode));
    
    for (const auto &fun : script->getAllFunctions())
        std::println("fun {}:\n{}", fun.name, NCSC::Compiler::disassemble(fun.bytecode));

    const NCSC::Function *fun = script->getFunction("caca");
    if (fun) {
        NCSC::VM vm(script);
        vm.computeGlobals();
        vm.prepareFunction(fun);

        if (!vm.setArguments(2, 3.0f, static_cast<uint8_t>(1))) {
            std::println("{}", vm.getLastError());
            exit(EXIT_FAILURE);
        }
        
        bool success = vm.execute();
        if (!success) {
            std::println("{}", vm.getLastError());
            exit(EXIT_FAILURE);
        }
        std::println("{}", vm.getStackStrRepr());

        float returned = 0;
        if (!vm.getFunctionReturn(returned)) {
            std::println("{}", vm.getLastError());
            exit(EXIT_FAILURE);
        }
        std::println("{} returned {}", fun->name, std::to_string(returned));
    }

    return 0;
}
