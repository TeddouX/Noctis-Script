// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"
#include "script_source.hpp"

#include <string>
#include <vector>
#include <unordered_map>
#include <memory>

namespace NCSC
{

class Bytecode {
public:
    struct DbgInfo {
        // Message from the compiler
        std::string mess;
        // Location in the source
        Location loc;
    };

    Bytecode() = default;

    explicit Bytecode(std::shared_ptr<ScriptSource> src)
        : src_(src) {}

    const std::shared_ptr<ScriptSource> getSrc() const { return src_; }
    const std::vector<Byte> &getBytes() const { return bytes_; }
    const DbgInfo &getDbgInfoAt(size_t byteIdx) const { return dbgInfo_.at(byteIdx); }

private:
    friend class Compiler;
    friend class Optimizer;

    std::shared_ptr<ScriptSource> src_;
    std::vector<Byte> bytes_;
    // Index into bytes -> whatever the compiler puts as debug info for that byte and a location in the source
    std::unordered_map<size_t, DbgInfo> dbgInfo_;
};

} // namespace NCSC
