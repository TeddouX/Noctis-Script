#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <ncsc/compiler.hpp>
#include <ncsc/vm.hpp>
#include <iostream>

int main() {
    std::string code = 
    "fun main() {\n"
    "   Int blabla = 1/2*3;\n"
    "   Int baaaaaa = 4+5-6;\n"
    "}\n";

    auto tokens = NCSC::Lexer(code).tokenizeAll();

    std::cout << code << std::endl;

    for (auto token : tokens) {
        std::cout << token.getStrRepr() << " Position: " << token.line << ":" << token.col << std::endl;
    }
    std::cout << "\n";

    NCSC::Parser parser(tokens);
    auto rootNode = parser.parseAll();
    if (parser.hasErrors()) {
        for (auto error : parser.getErrors()) 
            std::cout << error.getStrRepr() << std::endl;   
    }

    std::cout << rootNode.getStrRepr() << std::endl;

    NCSC::Compiler compiler;
    auto script = compiler.compileScript(rootNode);

    const NCSC::Function *fun = script->getFunction("main");
    if (fun) {
        std::cout << fun->getBytecodeStrRepr() << std::endl;

        NCSC::VM vm;
        vm.prepareFunction(fun);
        vm.execute();
    }

    return 0;
}
