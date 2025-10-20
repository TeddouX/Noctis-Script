#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <ncsc/compiler.hpp>
#include <ncsc/script_context.hpp>
#include <ncsc/vm.hpp>
#include <print>
#include <fstream>
#include <chrono>

void printHello() {
    std::println("Hello from CPP!");
}

int add(int a, int b) {
    return a + b;
}

int add4(int a, int b, int c, int d) {
    return a + b + c + d;
}

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

    auto scriptCtx = std::make_shared<NCSC::ScriptContext>();
    scriptCtx->registerGlobalFunction("printHello", printHello);
    scriptCtx->registerGlobalFunction("add", add);
    scriptCtx->registerGlobalFunction("add4", add4);

    NCSC::Compiler compiler(scriptCtx);
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

    const NCSC::ScriptFunction *fun = script->getFunction("main");
    if (fun) {
        NCSC::VM vm(script);
        vm.computeGlobals();
        vm.prepareFunction(fun);
        
        bool success = vm.execute();
        if (!success) {
            std::println("{}", vm.getLastError());
            exit(EXIT_FAILURE);
        }
        std::println("{}", vm.getStackStrRepr());
    }

    return 0;
}
