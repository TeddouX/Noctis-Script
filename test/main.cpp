#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <ncsc/compiler.hpp>
#include <iostream>

int main() {
    std::string code = 
    "fun main() {\n"
    "   Int blabla = 1 + 2 * 3 + 4 / 2;\n"
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

    std::cout << script->functions[0].getBytecodeStrRepr();

    return 0;
}

// #include <stdint.h>
// #include <iostream>

// typedef uint8_t   byte;
// typedef uint16_t  word;
// typedef uint32_t  dword;
// typedef uint64_t  qword;
// typedef uintptr_t ptrword;

// #define MAKE_INT_WORDS(i) (int64_t)i & 0xFFFF, ((int64_t)i >> 16) & 0xFFFF, ((int64_t)i >> 32) & 0xFFFF, ((int64_t)i >> 48) & 0xFFFF
// #define READ_ARG_INT() (int64_t)instructions[pc + 1] << 0 | (int64_t)instructions[pc + 2] << 16 | (int64_t)instructions[pc + 3] << 32 | (int64_t)instructions[pc + 4] << 48
// #define INSTRUCTION(instr) case (word)Instructions::instr: 
// #define END_INTRUCTION break

// enum class ValueType {
//     INT,
//     FLOAT,
//     REF,
// };

// struct StackValue {
//     ValueType type;
//     union { int64_t i; double f; ptrword ref; };
// };

// enum class Instructions : word {
//     PUSHINT, PUSHFLOAT, PUSHREF,
//     POP,
//     ADD,
// };

// int main() {
//     word instructions[] = {
//         (word)Instructions::PUSHINT, MAKE_INT_WORDS(20),
//         (word)Instructions::PUSHINT, MAKE_INT_WORDS(40),
//         (word)Instructions::ADD,
//     };
//     size_t instructionsSize = sizeof(instructions) / sizeof(word);
//     size_t pc = 0;

//     constexpr size_t stackSize = 2;
//     StackValue stack[stackSize];
//     size_t sp = 0;

//     while (pc < instructionsSize) {
//         switch (instructions[pc]) {
//             INSTRUCTION(PUSHINT) {
//                 StackValue val {
//                     .type = ValueType::INT,
//                     .i = READ_ARG_INT(),
//                 };

//                 // push(val)
//                 stack[sp] = val;
//                 sp++;

//                 pc += 5;
//                 END_INTRUCTION;
//             }

//             INSTRUCTION(ADD) {
//                 // pop()
//                 StackValue a = stack[--sp];
//                 StackValue b = stack[--sp];

//                 int64_t res = a.i + b.i;
//                 StackValue val {
//                     .type = ValueType::INT,
//                     .i = res,
//                 };

//                 // push(val)
//                 stack[sp] = val;
//                 sp++;

//                 pc++;
//                 END_INTRUCTION;
//             }
//         }
//     }

//     std::cout << stack[0].i << std::endl;
//     return 0;
// }
