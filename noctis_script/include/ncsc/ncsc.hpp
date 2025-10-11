#pragma once
#include <assert.h>
#include <stdfloat>
#include <stdint.h>

#if defined(_WIN32) || defined(_WIN64)
    // Disable warning from using STD library 
    // member variables in classes
    #pragma warning(disable : 4251)
    
    #ifdef NCSC_BUILD
        #define NCSC_API __declspec(dllexport)
    #else
        #define NCSC_API __declspec(dllimport)
    #endif
#else
    #define NCSC_API
#endif

namespace NCSC 
{

typedef uint8_t   Byte;
typedef uint16_t  Word;
typedef uint32_t  DWord;
typedef uint64_t  QWord;
typedef uintptr_t PtrWord;

typedef double float64_t;
typedef float  float32_t;

enum class Instruction : Byte {
    NOOP,       // NOOP ; Does nothing, used when compilation fails on an ast node

    STORELOCAL, // STORELOCAL 1 ; a ; Pops first value on the stack and stores it into local 1
    LOADLOCAL,  // LOADLOCAL 1 ; a ; Loads variable a onto the stack
    // PUSHCONST,  // PUSHCONST 1 ; pushes constant 1 onto the stack 

    PUSHINT,    // PUSHINT 120311321 ; pushes an int onto the stack
    PUSHFLOAT,  // PUSHFLOAT 1203.11321 ; pushes a float onto the stack

    POP,        // POP ; remove first value on the stack

    ADD,        // ADD ; pop first two values on the stack, adds them and pushes the result on the stack
    SUB,        // SUB ; pop first two values on the stack, substracts them and pushes the result on the stack
    MUL,        // MUL ; pop first two values on the stack, multiplies them and pushes the result on the stack
    DIV,        // DIV ; pop first two values on the stack, divides them and pushes the result on the stack

    RET,        // RET ; returns to the previous callframe on the callstack, pops temporary value and pushes it to caller
    RETVOID,    // RETVOID ; returns to the previous callframe on the callstack and removes locals and temporaries

    CALLSCRFUN, // CALLSCRFUN 0 ; calls script function at index 0
};

} // namespace NCSC
