// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"

#include <string>
#include <vector>

// INTERNAL
#define GETTERS_SETTERS_FOR_NAMED_VECTOR(name, vec, ty)                                                     \
    bool has##name(const std::string &name) { return get##name##Idx(name) != NCSC::INVALID_IDX; }           \
    void add##name(const ty& name) { vec.push_back(name); }                                                 \
    ty &emplace##name(const ty& name) { return vec.emplace_back(name); }                                    \
    ty *get##name(DWord idx) {                                                                              \
        if (idx >= vec.size())                                                                              \
            return nullptr;                                                                                 \
        return &vec[idx];                                                                                   \
    }                                                                                                       \
    ty *get##name(const std::string &name) { return NCSC::findNamed(vec, name); }                           \
    NCSC::DWord get##name##Idx(const std::string &name) const { return NCSC::getNamedIndex(vec, name); }    \
    std::vector<ty> &getAll##name##s() { return vec; }                                                      \
    NCSC::DWord get##name##Count() const { return vec.size(); }                                             \

// INTERNAL
#define GETTERS_SETTERS_FOR_NAMED_VECTOR_CASTS(name, vec, ty)                                               \
    bool has##name(const std::string &name) { return get##name##Idx(name) != NCSC::INVALID_IDX; }           \
    void add##name(const ty& name) { vec.push_back(name); }                                                 \
    ty &emplace##name(const ty& name) { return static_cast<ty &>(vec.emplace_back(name)); }                 \
    ty *get##name(DWord idx) {                                                                              \
        if (idx >= vec.size())                                                                              \
            return nullptr;                                                                                 \
        return static_cast<ty *>(&vec[idx]);                                                                \
    }                                                                                                       \
    ty *get##name(const std::string &name) { return static_cast<ty *>(NCSC::findNamed(vec, name)); }        \
    NCSC::DWord get##name##Idx(const std::string &name) const { return NCSC::getNamedIndex(vec, name); }    \
    std::vector<ty> getAll##name##s() { return NCSC::convertVector<ty>(vec); }                             \
    NCSC::DWord get##name##Count() const { return vec.size(); }                                             \


namespace NCSC
{
    
template <typename Inherited_, typename Base_>
requires(std::is_base_of_v<Base_, Inherited_>)
std::vector<Inherited_> convertVector(const std::vector<Base_> &baseVector) {
    std::vector<Inherited_> res;
    for (const Base_ &b : baseVector)
        res.push_back(static_cast<const Inherited_ &>(b));
    return res;
}

// Satisfied by a type that contains a member named "name" 
template <typename T>
concept IsNamed = requires(T t) {
    t.name;
};

template <typename T>
requires(IsNamed<T>)
T* findNamed(std::vector<T>& vec, const std::string& name) {
    for (auto& v : vec)
        if (v.name == name)
            return &v;
    return nullptr;
}

// Returs INVALID_IDX if the search isn't conclusive
template <typename T>
requires(IsNamed<T>)
DWord getNamedIndex(const std::vector<T>& vec, const std::string& name) {
    for (size_t i = 0; i < vec.size(); ++i)
        if (vec[i].name == name)
            return static_cast<DWord>(i);
    return INVALID_IDX;
}

} // namespace NCSC
