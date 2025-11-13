// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"

#include <unordered_map>

namespace NCSC
{
    
enum class Instruction : Byte {
    NOOP,           // NOOP ; Does nothing, used when compilation fails on an ast node

    PUSH,           // PUSH ; [DWord for value type][depends on the value type]
    POP,            // PUSH ; [DWord for value type][depends on the value type]
    DUP,            // DUP ; Duplicates last value on the stack

    LOADLOCAL,      // LOADLOCAL 1 ; Loads a local variable a onto the stack
    STORELOCAL,     // STORELOCAL 1 ; Pops value and sets local 1 to it
    
    LOADGLOBAL,     // LOADLOCAL 1 ; Loads a global variable a onto the stack
    STOREGLOBAL,    // STOREGLOBAL 1 ; Pops value and sets global 1 to it

    LOADMEMBER,     // LOADMEMBER 1 ; Loads a member variable from the last object pushed on the stack, if none exists, try loading it from the object register
    STOREMEMBER,    // STOREMEMBER 1 ; Pops value and sets member 1 to it

    ADD,            // ADD ; pop first two values on the stack, adds them and pushes the result on the stack
    SUB,            // SUB ; pop first two values on the stack, substracts them and pushes the result on the stack
    MUL,            // MUL ; pop first two values on the stack, multiplies them and pushes the result on the stack
    DIV,            // DIV ; pop first two values on the stack, divides them and pushes the result on the stack
    INC,            // INC ; pop first value on the stack, increments it and pushes the result on the stack
    DEC,            // DEC ; pop first value on the stack, decrements it and pushes the result on the stack
    NOT,            // DEC ; pop first value on the stack, inverts its boolean value, and pushes the result on the stack

    CMPST,          // CMPSM ; pops last two values on the stack, and pushes true is first is smaller than the second
    CMPSE,          // CMPSE ; pops last two values on the stack, and pushes true is first is smaller or equal than the second
    CMPGT,          // CMPGT ; pops last two values on the stack, and pushes true is first is greater than the second
    CMPGE,          // CMPGE ; pops last two values on the stack, and pushes true is first is greater or equal than the second
    CMPEQ,          // CMPEQ ; pops last two values on the stack, and pushes true if the two are equal
    CMPNE,          // CMPNE ; pops last two values on the stack, and pushes true if the two are not equal

    JMP,            // JMP 123 ; set the PC to the operand
    JMPFALSE,       // JMPFALSE 123 ; if the last value on the stack is false or equals to zero, set the PC to the operand
    JMPTRUE,        // JMPTRUE 123 ; if the last value on the stack is true or different to zero, set the PC to the operand

    TYCAST,         // TYCAST b ; pops the last value on the stack and changes it type to b 

    RET,            // RET ; returns to the previous callframe on the callstack, pops temporary value and pushes it to caller
    RETVOID,        // RETVOID ; returns to the previous callframe on the callstack and removes locals and temporaries

    CALLSCRFUN,     // CALLSCRFUN 0 ; calls script function at index 0
    CALLCPPFUN,     // CLGLBLCPPFUN 0 ; calls a global function registered in the ScriptContext at index 0
    
    CALLMETHOD,     // CALLMETHOD 0 ; calls method at index 0
    CALLCPPMETHOD,  // CLCPPMETHOD 0 ; calls method at index 0

    NEW,            // NEW 0 ; creates object of type 0
    CPPNEW,         // CPPNEW 0 ; creates cpp object of idx 0

    LABEL,          // Temporary instruction to indicate the location of a jump
};

// Instruction -> (name & operand size)
const std::unordered_map<Instruction, std::pair<const char *, size_t>> INSTR_INFO = {
    { Instruction::NOOP,            {"NOOP",           0} },

    { Instruction::PUSH,            {"PUSH",           sizeof(DWord)} },
    { Instruction::POP,             {"POP",            0} },
    { Instruction::DUP,             {"DUP",            0} },
    
    { Instruction::LOADLOCAL,       {"LOADLOCAL",      sizeof(DWord)} },
    { Instruction::STORELOCAL,      {"STORELOCAL",     sizeof(DWord)} },

    { Instruction::LOADGLOBAL,      {"LOADGLOBAL",     sizeof(DWord)} },
    { Instruction::STOREGLOBAL,     {"STOREGLOBAL",    sizeof(DWord)} },

    { Instruction::LOADMEMBER,      {"LOADMEMBER",     sizeof(DWord)} },
    { Instruction::STOREMEMBER,     {"STOREMEMBER",    sizeof(DWord)} },
    
    { Instruction::ADD,             {"ADD",            0} },
    { Instruction::SUB,             {"SUB",            0} },
    { Instruction::MUL,             {"MUL",            0} },
    { Instruction::DIV,             {"DIV",            0} },
    { Instruction::INC,             {"INC",            0} },
    { Instruction::DEC,             {"DEC",            0} },
    { Instruction::NOT,             {"NOT",            0} },

    { Instruction::CMPST,           {"CMPST",          0} },
    { Instruction::CMPSE,           {"CMPSE",          0} },
    { Instruction::CMPGT,           {"CMPGT",          0} },
    { Instruction::CMPGE,           {"CMPGE",          0} },
    { Instruction::CMPEQ,           {"CMPEQ",          0} },
    { Instruction::CMPNE,           {"CMPNE",          0} },

    { Instruction::JMP,             {"JMP",            sizeof(QWord)} },
    { Instruction::JMPFALSE,        {"JMPFALSE",       sizeof(QWord)} },
    { Instruction::JMPTRUE,         {"JMPTRUE",        sizeof(QWord)} },

    { Instruction::TYCAST,          {"TYCAST",         sizeof(DWord)} },
    
    { Instruction::RET,             {"RET",            0} },
    { Instruction::RETVOID,         {"RETVOID",        0} },

    { Instruction::CALLSCRFUN,      {"CALLSCRFUN",     sizeof(DWord)} },
    { Instruction::CALLCPPFUN,      {"CALLCPPFUN",     sizeof(DWord)} },

    { Instruction::CALLMETHOD,      {"CALLMETHOD",     sizeof(QWord)} /* DWord -> Object idx + DWord -> Method idx */ },
    { Instruction::CALLCPPMETHOD,   {"CALLCPPMETHOD",  sizeof(QWord)} },

    { Instruction::NEW,             {"NEW",            sizeof(DWord)} },
    { Instruction::CPPNEW,          {"CPPNEW",         sizeof(DWord)} },

    { Instruction::LABEL,           {"LABEL",          sizeof(QWord)} },
};

size_t NCSC_API getInstructionSize(const std::vector<Byte> &bytes, size_t off);

} // namespace NCSC
