#pragma once
#include "function.hpp"
#include "script.hpp"
#include "ncsc.hpp"

#include <memory>

#define MAKE_INT_WORDS(i) (int64_t)i & 0xFFFF, ((int64_t)i >> 16) & 0xFFFF, ((int64_t)i >> 32) & 0xFFFF, ((int64_t)i >> 48) & 0xFFFF
#define MAKE_FLOAT_WORDS(i) (double)i & 0xFFFF, ((double)i >> 16) & 0xFFFF, ((double)i >> 32) & 0xFFFF, ((double)i >> 48) & 0xFFFF

namespace NCSC
{
    
enum class Instruction : Byte {
    STORELOCAL, // STORELOCAL 1 ; a ; Pops first value on the stack and stores it into local 1
    LOADLOCAL,  // LOADLOCAL 1 ; a ; Loads variable a onto the stack
    PUSHCONST,  // PUSHCONST 1 ; pushes constant 1 onto the stack 
    PUSHINT,    // PUSHINT 120311321 ; pushes an int onto the stack
    PUSHFLOAT,  // PUSHFLOAT 1203.11321 ; pui
    POP,        // POP ; remove first value on the stack
    ADD,        // ADD ; pop first two values on the stack, adds them and pushes the result on the stack
};

class NCSC_API Compiler {
public:
    Compiler() = default;

    std::shared_ptr<Script> compileScript(const ScriptNode &root);

private:
    Script   *currScript_;
    Function *currFunction_;

    // currFunction = function and at the end set it to nullptr to indicate that it has finished compilation
    void compileFunction(const ScriptNode &funcDecl);

    // Computes required stack size for a node
    size_t computeMaxStackSize(const ScriptNode &node);

    // Add a byte to the bytecode of the current function
    void emit(Byte bytecode) { currFunction_->bytecode.push_back(bytecode); }
    void emit(Byte *bytecode, size_t size);
    void emitInt16(int16_t i);

    // Add an instruction to the bytecode of the current function
    void emit(Instruction instr) { currFunction_->bytecode.push_back(static_cast<Byte>(instr)); }

    void compileVariableDeclaration(const ScriptNode &varDecl);
};

} // namespace NCSC
