#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <iostream>

int main() {
    auto a = NCSC::Lexer("float a;").tokenizeAll();
    auto b = NCSC::Parser(a);
    b.parseAll();

    return 0;
}