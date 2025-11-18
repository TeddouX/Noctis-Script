// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <string>
#include <vector>
#include <filesystem>
#include <memory>

#include "ncsc.hpp"

namespace NCSC
{

class NCSC_API ScriptSource {
public:
    static std::shared_ptr<ScriptSource> fromSource(const std::string &source);

    // Optional
    std::string fileName = "";
    std::filesystem::path filePath;

    std::string getLine(size_t line);
    const std::string &getString() const { return string_; }
    size_t numLines() const { return lines_.size(); }

private:
    explicit ScriptSource(const std::string &source);

    std::vector<std::pair<size_t, size_t>> lines_;
    std::string string_;
};

} // namespace NCSC
