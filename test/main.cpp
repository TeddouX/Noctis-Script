#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <iostream>

int main() {
    std::string code = "int a = 1;\nint b = 2 int z;";
    auto a = NCSC::Lexer(code).tokenizeAll();

    std::cout << code << std::endl;

    // for (auto c : a) {
    //     std::cout << c.getStrRepr() << " Position: " << c.line << ":" << c.col << std::endl;
    // }
    
    auto b = NCSC::Parser(a);

    b.parseAll();
    if (b.hasErrors()) {
        for (auto c : b.getErrors()) 
            std::cout << c.getStrRepr();
        
        return 1;
    }

    return 0;
}