// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"

namespace NCSC
{
    
enum class Instruction : Byte {
    NOOP,         // NOOP ; Does nothing, used when compilation fails on an ast node

    PUSH,         // PUSH ; [DWord for value type][depends on the value type]
    POP,          // PUSH ; [DWord for value type][depends on the value type]

    LOADLOCAL,    // LOADLOCAL 1 ; Loads a local variable a onto the stack and stores a ptr to it in the obj register
    STORELOCAL,   // STORELOCAL 1 ; Pops last value on the stack it and stores it into local 1
    LOADGLOBAL,   // LOADLOCAL 1 ; Loads a global variable a onto the stack and stores a ptr to it in the obj register
    STOREGLOBAL,  // STOREGLOBAL 1 ; Pops last value on the stack it and stores it into global 1

    ADD,          // ADD ; pop first two values on the stack, adds them and pushes the result on the stack
    SUB,          // SUB ; pop first two values on the stack, substracts them and pushes the result on the stack
    MUL,          // MUL ; pop first two values on the stack, multiplies them and pushes the result on the stack
    DIV,          // DIV ; pop first two values on the stack, divides them and pushes the result on the stack

    CMPST,        // CMPSM ; pops last two values on the stack, and pushes true is first is smaller than the second
    CMPSE,        // CMPSE ; pops last two values on the stack, and pushes true is first is smaller or equal than the second
    CMPGT,        // CMPGT ; pops last two values on the stack, and pushes true is first is greater than the second
    CMPGE,        // CMPGE ; pops last two values on the stack, and pushes true is first is greater or equal than the second
    CMPEQ,        // CMPEQ ; pops last two values on the stack, and pushes true if the two are equal
    CMPNE,        // CMPNE ; pops last two values on the stack, and pushes true if the two are not equal

    JMP,          // JMP 123 ; set the PC to the operand
    JMPFALSE,     // JMPFALSE 123 ; if the last value on the stack is false or equals to zero, set the PC to the operand

    SETOBJ,       // SETOBJ ; pops last value on the stack and sets it to the object stored in the object register  
    LOADOBJ,      // LOADOBJ ; pushes the object stored in the object register  

    TYCAST,       // TYCAST b ; pops the last value on the stack and changes it type to b 

    RET,          // RET ; returns to the previous callframe on the callstack, pops temporary value and pushes it to caller
    RETVOID,      // RETVOID ; returns to the previous callframe on the callstack and removes locals and temporaries

    CALLSCRFUN,   // CALLSCRFUN 0 ; calls script function at index 0
    CLGLBLCPPFUN, // CLGLBLCPPFUN 0 ; calls a global function registered in the ScriptContext at index 0
};

// Instruction -> (name & operand size)
const std::unordered_map<Instruction, std::pair<const char *, size_t>> INSTR_INFO = {
    { Instruction::PUSH,          {"PUSH",         sizeof(DWord)} },
    { Instruction::POP,           {"POP",          0} },
    
    { Instruction::LOADLOCAL,     {"LOADLOCAL",    sizeof(DWord)} },
    { Instruction::STORELOCAL,    {"STORELOCAL",   sizeof(DWord)} },
    { Instruction::LOADGLOBAL,    {"LOADGLOBAL",   sizeof(DWord)} },
    { Instruction::STOREGLOBAL,   {"STOREGLOBAL",  sizeof(DWord)} },

    { Instruction::CALLSCRFUN,    {"CALLSCRFUN",   sizeof(DWord)} },
    { Instruction::CLGLBLCPPFUN,  {"CLGLBLCPPFUN", sizeof(DWord)} },
    
    { Instruction::ADD,           {"ADD",          0} },
    { Instruction::SUB,           {"SUB",          0} },
    { Instruction::MUL,           {"MUL",          0} },
    { Instruction::DIV,           {"DIV",          0} },

    { Instruction::CMPST,         {"CMPST",        0} },
    { Instruction::CMPSE,         {"CMPSE",        0} },
    { Instruction::CMPGT,         {"CMPGT",        0} },
    { Instruction::CMPGE,         {"CMPGE",        0} },
    { Instruction::CMPEQ,         {"CMPEQ",        0} },
    { Instruction::CMPNE,         {"CMPNE",        0} },

    { Instruction::JMP,           {"JMP",          sizeof(QWord)} },
    { Instruction::JMPFALSE,      {"JMPFALSE",     sizeof(QWord)} },
    
    { Instruction::SETOBJ,        {"SETOBJ",       0} },
    { Instruction::LOADOBJ,       {"LOADOBJ",      0} },

    { Instruction::TYCAST,        {"TYCAST",       sizeof(DWord)} },
    
    { Instruction::RET,           {"RET",          0} },
    { Instruction::RETVOID,       {"RETVOID",      0} },

    { Instruction::NOOP,          {"NOOP",         0} },
};

} // namespace NCSC
