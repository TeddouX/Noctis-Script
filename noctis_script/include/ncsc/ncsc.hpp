#pragma once
#include <assert.h>
#include <stdint.h>
#include <unordered_map>
#include <string>

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

#define NCSC_INVALID_IDX (DWord)(-1)

typedef double float64_t;
typedef float  float32_t;

enum class Instruction : Byte {
    NOOP,        // NOOP ; Does nothing, used when compilation fails on an ast node

    STORELOCAL,  // STORELOCAL 1 ; a ; Pops first value on the stack and stores it into local 1
    LOADLOCAL,   // LOADLOCAL 1 ; a ; Loads variable a onto the stack

    STOREGLOBAL, // STORELOCAL 1 ; Pops first value on the stack and stores it into global 1 
    LOADGLOBAL,  // LOADLOCAL 1 ; a ; Loads a global variable a onto the stack

    PUSH,        // PUSH ; [DWord for value type][depends on the value type]

    ADD,         // ADD ; pop first two values on the stack, adds them and pushes the result on the stack
    SUB,         // SUB ; pop first two values on the stack, substracts them and pushes the result on the stack
    MUL,         // MUL ; pop first two values on the stack, multiplies them and pushes the result on the stack
    DIV,         // DIV ; pop first two values on the stack, divides them and pushes the result on the stack

    RET,         // RET ; returns to the previous callframe on the callstack, pops temporary value and pushes it to caller
    RETVOID,     // RETVOID ; returns to the previous callframe on the callstack and removes locals and temporaries

    CALLSCRFUN,  // CALLSCRFUN 0 ; calls script function at index 0
};

// Instruction -> (name & operand size)
const std::unordered_map<Instruction, std::pair<const char *, size_t>> INSTR_INFO = {
    { Instruction::PUSH,        {"PUSH",        sizeof(DWord)} },
    
    { Instruction::LOADGLOBAL,  {"LOADGLOBAL",  sizeof(DWord)} },
    { Instruction::STOREGLOBAL, {"STOREGLOBAL", sizeof(DWord)} },
    { Instruction::CALLSCRFUN,  {"CALLSCRFUN",  sizeof(DWord)} },
    
    { Instruction::STORELOCAL,  {"STORELOCAL",  sizeof(Word)} },
    { Instruction::LOADLOCAL,   {"LOADLOCAL",   sizeof(Word)} },
    
    { Instruction::ADD,         {"ADD",         0} },
    { Instruction::SUB,         {"SUB",         0} },
    { Instruction::MUL,         {"MUL",         0} },
    { Instruction::DIV,         {"DIV",         0} },
    
    { Instruction::RET,         {"RET",         0} },
    { Instruction::RETVOID,     {"RETVOID",     0} },

    { Instruction::NOOP,        {"NOOP",        0} },
};

enum class ValueType : NCSC::DWord {
    INVALID = 0x0,

    INT16   = 0x1,
    INT32   = 0x2,
    INT64   = 0x3,

    UINT16   = 0x4,
    UINT32   = 0x5,
    UINT64   = 0x6,

    FLOAT32 = 0x7,
    FLOAT64 = 0x8,

    BOOL    = 0x9,
};

const std::unordered_map<ValueType, const char *> VTYPE_NAMES = {
    { ValueType::INT16, "Int16" }, 
    { ValueType::INT32, "Int32" }, 
    { ValueType::INT64, "Int64" }, 

    { ValueType::UINT16, "UInt16" }, 
    { ValueType::UINT32, "UInt32" }, 
    { ValueType::UINT64, "UInt64" }, 

    { ValueType::FLOAT32, "FLoat32" }, 
    { ValueType::FLOAT64, "FLoat64" }, 

    { ValueType::BOOL, "Bool" }, 
};

template <typename T>
inline T readWord(const Byte *bytes, size_t idx) {
    using U = std::make_unsigned_t<T>; 
    U words = 0;
    for (size_t i = 0; i < sizeof(T); ++i)
        words |= (static_cast<U>(bytes[idx + i]) << (i * 8));
    return static_cast<T>(words);
}

template <typename T>
inline void makeBytes(const T &val, Byte *bytes, size_t off = 0) {
    for (int i = 0; i < sizeof(T); ++i)
        bytes[off + i] = static_cast<Byte>((val >> (i * 8)) & 0xFF);
}

template <typename T>
inline void makeValueBytes(const T &val, ValueType ty, Byte *bytes, size_t off = 0) {
    // VM will reverse this order when pushind to the stack
    // ValueType bytes first
    makeBytes(static_cast<DWord>(ty), bytes, off);
    // val bytes last
    makeBytes(val, bytes, off + sizeof(ValueType));
}

template <typename T>
inline constexpr size_t getValueSize(const T &) {
    return sizeof(ValueType) + sizeof(T); 
}

} // namespace NCSC
