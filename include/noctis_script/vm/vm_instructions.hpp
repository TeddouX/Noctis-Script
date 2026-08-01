#pragma once
#include "../ncsc.hpp"

namespace NCSC
{

using vm_instruction_size_t = byte_t;

enum class VMInstruction : vm_instruction_size_t
{
    NOOP,           // NOOP ; Does nothing, used when compilation fails on an ast node

    PUSH,           // PUSH
    POP,            // PUSH
    DUP,            // DUP ; Duplicates last value on the stack

    LOADLOCAL,      // LOADLOCAL 1 ; Loads a local variable a onto the stack
    STORELOCAL,     // STORELOCAL 1 ; Pops value and sets local 1 to it
    
    LOADGLOBAL,     // LOADLOCAL 1 ; Loads a global variable a onto the stack
    STOREGLOBAL,    // STOREGLOBAL 1 ; Pops value and sets global 1 to it

    LOADMEMBER,     // LOADMEMBER 1 ; Loads a member variable from the last object pushed on the stack
    STOREMEMBER,    // STOREMEMBER 1 ; Pops value and sets member 1 to it

    ADD,            // ADD ; pop first two values on the stack, adds them and pushes the result on the stack
    SUB,            // SUB ; pop first two values on the stack, substracts them and pushes the result on the stack
    MUL,            // MUL ; pop first two values on the stack, multiplies them and pushes the result on the stack
    DIV,            // DIV ; pop first two values on the stack, divides them and pushes the result on the stack
    INC,            // INC ; pop first value on the stack, increments it and pushes the result on the stack
    DEC,            // DEC ; pop first value on the stack, decrements it and pushes the result on the stack
    NOT,            // DEC ; pop first value on the stack, inverts its boolean value, and pushes the result on the stack

    CMPST,          // CMPST ; pops last two values on the stack, and pushes true is first is smaller than the second
    CMPSE,          // CMPSE ; pops last two values on the stack, and pushes true is first is smaller or equal than the second
    CMPGT,          // CMPGT ; pops last two values on the stack, and pushes true is first is greater than the second
    CMPGE,          // CMPGE ; pops last two values on the stack, and pushes true is first is greater or equal than the second
    CMPEQ,          // CMPEQ ; pops last two values on the stack, and pushes true if the two are equal
    CMPNE,          // CMPNE ; pops last two values on the stack, and pushes true if the two are not equal

    JMP,            // JMP 123 ; set the PC to the operand
    JMPFALSE,       // JMPFALSE 123 ; if the last value on the stack is false or equals to zero, set the PC to the operand
    JMPTRUE,        // JMPTRUE 123 ; if the last value on the stack is true or different to zero, set the PC to the operand

    RET,            // RET ; returns to the previous callframe on the callstack, pops temporary value and pushes it to caller
    RETVOID,        // RETVOID ; returns to the previous callframe on the callstack and removes locals and temporaries

    CALLFUNC,       // CALLFUNC 0 ; calls script function at index 0

    NEW,            // NEW 0 ; creates object located at index 0

    LABEL,          // Used by the compiler to resolve jumps
};

} // namespace NCSC
