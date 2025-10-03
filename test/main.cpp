#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <iostream>

int main() {
    auto a = NCSC::Lexer("a;\nint;\nfloat;\n23;\n").tokenizeAll();

    for (auto c : a) {
        std::cout << c.getStrRepr() << " Position: " << c.line << ":" << c.col << std::endl;
    }
    
    auto b = NCSC::Parser(a);
    b.parseAll();

    return 0;
}