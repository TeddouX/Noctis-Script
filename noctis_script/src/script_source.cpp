// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/script_source.hpp>
#include <sstream>
#include <format>

namespace NCSC
{
    
std::shared_ptr<ScriptSource> ScriptSource::fromSource(const std::string &source) {
    return std::shared_ptr<ScriptSource>(new ScriptSource(source));
}

std::string ScriptSource::getLine(size_t line) {
    if (line == 0 || line > lines_.size())
        return std::format("Line too small or too big ({} > {})", 
            std::to_string(line), 
            std::to_string(lines_.size())); 
    
    auto offset = lines_[line - 1];
    return string_.substr(offset.first, offset.second);
}

ScriptSource::ScriptSource(const std::string &source)
    : string_(source) 
{
    size_t start = 0;
    for (size_t i = 0; i < source.size(); i++) {
        bool isNL = source[i] == '\n';
        bool isLastChar = i + 1 >= source.size();
        
        if (isNL || isLastChar) {
            lines_.push_back({ 
                start, 
                (isLastChar && !isNL) ? i + 1 - start: i - start }
            );
            start = i + 1;

            if (isLastChar && isNL) {
                // Add a space if the source ends with an empty line
                string_ += ' ';
                i++;
                
                lines_.push_back({ start, i - start });
            }
        }
    }
}

} // namespace NCSC

