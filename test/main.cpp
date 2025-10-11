#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <ncsc/compiler.hpp>
#include <ncsc/vm.hpp>
#include <print>

int main() {
    std::string code = 
    "Int hello() {\n"
    "   return 1+1;\n"
    "}\n"
    "fun main() {\n"
    // "   hello();\n"
    "   Int baaaaaa = hello() + 5 - 6;\n"
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

    const NCSC::Function *fun = script->getFunction("main");
    if (fun) {
        std::println("fun {}:\n{}", fun->name, fun->getBytecodeStrRepr());

        NCSC::VM vm(script);
        vm.prepareFunction(fun);
        vm.execute();
    }

    return 0;
}
