// The code below is not covered by the project's main license (BSD 2-Clause). 
// It can be used freely without including the license.
#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <ncsc/compiler.hpp>
#include <ncsc/script_context.hpp>
#include <ncsc/vm.hpp>
#include <print>
#include <fstream>
#include <chrono>

void printHello(int i) {
    std::println("Hello from CPP! {}", i);
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
    std::filesystem::path filePath = std::filesystem::absolute("test/code.ncsc");
    ifs.open(filePath);
    buf << ifs.rdbuf();

    std::string fileContents = buf.str();

    std::println("{}", fileContents);

    auto src = NCSC::ScriptSource::fromSource(fileContents);
    src->filePath = filePath;

    auto tokens = NCSC::Lexer(src).tokenizeAll();
    NCSC::Parser parser(tokens, src);
    auto rootNode = parser.parseAll();
    std::println("{}", rootNode.getStrRepr());
    if (parser.hasErrors()) {
        for (auto error : parser.getErrors()) 
            std::println("{}", error.getErrorMessage());

        exit(EXIT_FAILURE);
    }

    std::shared_ptr<NCSC::ScriptContext> scriptCtx = NCSC::ScriptContext::create();
    scriptCtx->registerGlobalFunction("printHello", printHello);
    scriptCtx->registerGlobalFunction("add", add);
    scriptCtx->registerGlobalFunction("add4", add4);

    NCSC::Compiler compiler(scriptCtx);
    std::shared_ptr<NCSC::Script> script = compiler.compileScript(src);
    if (compiler.hasErrors()) {
        for (auto error : compiler.getErrors())
            std::println("{}", error.getErrorMessage());

        exit(EXIT_FAILURE);
    }

    for (const auto &var : script->getAllGlobalVariables())
        std::println("global {}:\n{}", var.name, NCSC::Compiler::disassemble(var.bytecode));
    
    for (const auto &fun : script->getAllFunctions())
        std::println("fun {}:\n{}", fun.name, NCSC::Compiler::disassemble(fun.bytecode));

    const NCSC::ScriptFunction *fun = script->getFunction("main");
    if (fun) {
        NCSC::VM vm(script);
        if (!vm.computeGlobals()) {
            std::println("{}", vm.getLastError());
            exit(EXIT_FAILURE);
        }

        vm.prepareFunction(fun);

        if (!vm.execute()) {
            std::println("{}", vm.getLastError());
            exit(EXIT_FAILURE);
        }
        
        std::println("{}", vm.getStackStrRepr());
    }

    return 0;
}
