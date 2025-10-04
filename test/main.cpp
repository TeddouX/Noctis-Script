#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <iostream>

int main() {
    std::string code = "Int a(Int what, Float whatelse) {\n\tInt a = 0;\n\tFloat b = 1.21122121;\n\tFloat c = ; \n}\n";
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