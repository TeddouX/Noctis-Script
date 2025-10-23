/*
** SPDX-License-Identifier: BSD-2-Clause
** Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
**
** Note: The SPDX identifier above implies the full text of the BSD 2-Clause License included below.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are met:
** 
** 1. Redistributions of source code must retain the above copyright notice, this
**    list of conditions and the following disclaimer.
** 
** 2. Redistributions in binary form must reproduce the above copyright notice,
**    this list of conditions and the following disclaimer in the documentation
**    and/or other materials provided with the distribution.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
** AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
** DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
** FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
** DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
** SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
** CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
** OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#pragma once
#undef NDEBUG // Make assertions work in release builds
#include <assert.h>
#include <stdint.h>
#include <cstring>
#include <type_traits>
#include <vector>

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
// I recommend always setting this to true
#define NCSC_ALWAYS_OPTIMIZE         true

namespace NCSC 
{

typedef uint8_t   Byte;
typedef uint16_t  Word;
typedef uint32_t  DWord;
typedef uint64_t  QWord;
typedef uintptr_t PtrWord;

constexpr DWord NCSC_INVALID_IDX = -1;

typedef double float64_t;
typedef float  float32_t;

template <typename T>
inline T readWord(const std::vector<Byte> &bytes, size_t idx) {
    assert(idx + sizeof(T) <= bytes.size());
#if NCSC_USE_UNSAFE_WORD_READING
    return *reinterpret_cast<const T*>(bytes + idx);
#else
    T val{};
    std::memcpy(&val, bytes.data() + idx, sizeof(T));
    return val;
#endif
}

template <typename T>
requires std::is_trivially_copyable_v<T>
inline void makeBytes(const T &val, std::vector<Byte> &bytes, size_t off = 0) {
    assert(sizeof(T) + off <= bytes.size());
    std::memcpy(bytes.data() + off, &val, sizeof(T));
}

} // namespace NCSC
