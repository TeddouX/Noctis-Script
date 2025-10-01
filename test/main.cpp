#include <ncsc/lexer.h>
#include <iostream>

int main() {
    auto a = NCSC::Lexer("float a = 1.1;").tokenizeAll();

    for (auto b : a)
        std::cout << "Type: " << (int)b.type << " Value: \"" << b.val << "\"" << std::endl;

    return 0;
}