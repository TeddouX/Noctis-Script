// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"
#include "script_source.hpp"
#include "ast_node.hpp"

#include <string>
#include <vector>
#include <unordered_map>
#include <memory>

namespace NCSC
{

class NCSC_API Bytecode {
public:
    struct DbgInfo {
        // Message from the compiler
        std::string mess;

        Location loc;

        ASTNodeType nodeType;
    };

    struct LocationEntry {
        size_t offset;
        Location loc;
    };

    Bytecode() = default;

    explicit Bytecode(std::shared_ptr<ScriptSource> src, bool hasDebugInfo = false)
        : src_(src), hasDbgInfo_(hasDebugInfo) {}

    const std::shared_ptr<ScriptSource> getSrc() const { return src_; }
    const std::vector<Byte> &getBytes() const { return bytes_; }
    const DbgInfo *getDbgInfoAt(size_t byteIdx) const { return dbgInfo_.find(byteIdx) == dbgInfo_.end() ? nullptr : &dbgInfo_.at(byteIdx); }
    const Location &getLocationAt(size_t byteIdx) const;
    bool hasDebugInfo() const { return hasDbgInfo_; }

private:
    friend class Compiler;
    friend class Optimizer;

    std::shared_ptr<ScriptSource> src_;
    std::vector<Byte> bytes_;
    // Index into bytes -> whatever the compiler puts as debug info for that byte
    std::unordered_map<size_t, DbgInfo> dbgInfo_;
    std::vector<LocationEntry> locationEntries_;
    bool hasDbgInfo_ = false;
};

} // namespace NCSC
