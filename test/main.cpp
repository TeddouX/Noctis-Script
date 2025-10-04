#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <iostream>

int main() {
    std::string code = 
    "Int thisIsAGlobalVariable = 1 + 2 + 3 + 4 + 5;\n"
    "Int WhateverFunctionFoo(Int foo, Float bar, Int whatever) {\n"
    "   Int blabla = 1 + 1;\n"
    "   Float blablabla = 2.0 + 30.59;\n"
    "   Int whhhhhaaaattt = 3;\n"
    "}\n";

    auto a = NCSC::Lexer(code).tokenizeAll();

    std::cout << code << std::endl;
    
    for (auto c : a) {
        std::cout << c.getStrRepr() << " Position: " << c.line << ":" << c.col << std::endl;
    }
    std::cout << "\n";
    
    auto b = NCSC::Parser(a);
    auto d = b.parseAll();
    if (b.hasErrors()) {
        for (auto c : b.getErrors()) 
            std::cout << c.getStrRepr() << std::endl;   
    }

    std::cout << d.getStrRepr() << std::endl;

    return 0;
}