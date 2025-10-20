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

// Faster but may cause problems if unaligned
#define NCSC_USE_UNSAFE_WORD_READING false

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

template <typename T>
inline T readWord(const Byte *bytes, size_t idx) {
#if NCSC_USE_UNSAFE_WORD_READING
    return *reinterpret_cast<const T*>(bytes + idx);
#else
    T val{};
    std::memcpy(&val, bytes + idx, sizeof(T));
    return val;
#endif
}

template <typename T>
inline void makeBytes(const T &val, Byte *bytes, size_t off = 0) {
    for (int i = 0; i < sizeof(T); ++i)
        bytes[off + i] = static_cast<Byte>((val >> (i * 8)) & 0xFF);
}

} // namespace NCSC
