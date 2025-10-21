// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/script_source.hpp>
#include <sstream>

namespace NCSC
{
    
std::shared_ptr<ScriptSource> ScriptSource::fromSource(const std::string &source) {
    return std::shared_ptr<ScriptSource>(new ScriptSource(source));
}

std::string ScriptSource::getLine(size_t line) {
    if (line == 0 || line > lines_.size())
        return "line too small or too big";
    
    auto offset = lines_[line - 1];
    return string_.substr(offset.first, offset.second);
}

ScriptSource::ScriptSource(const std::string &source)
    : string_(source) 
{
    size_t start = 0;
    for (size_t i = 0; i < source.size(); i++) {
        const char &charAt = source[i];
        if (charAt == '\n') {
            lines_.push_back({ start, i - start });
            start = i + 1;
        }
    }
}

} // namespace NCSC

