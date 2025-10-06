#pragma once
#include <assert.h>
#include <stdfloat>
#include <stdint.h>

typedef uint8_t   Byte;
typedef uint16_t  Word;
typedef uint32_t  DWord;
typedef uint64_t  QWord;
typedef uintptr_t PtrWord;

enum class ValueType {
    INT,
    FLOAT,
};

struct Value {
    ValueType type;
    union { 
        int64_t i; 
        double f;
    };
};

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
