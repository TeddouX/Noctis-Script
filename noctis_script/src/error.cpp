// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/error.hpp>
#include <format>
#include <sstream>

namespace NCSC
{

static constexpr int ANSI_RED = 31;

static std::string formatSrcLineNumber(size_t lineNum) {
    return std::to_string(lineNum) + " | ";
}

static std::string formatColor(int col, bool bold, std::string str) {
    std::stringstream sstream;

    sstream << std::format("\033[{}", col);
    if (bold)
        sstream << ";1";
    sstream << std::format("m{}\033[0m", str);
    
    return sstream.str();
}

Error::Error(const ErrInfo &errInfo, std::shared_ptr<ScriptSource> src)
    : info_(errInfo), src_(src) 
{
    assert(src != nullptr);
}

std::string Error::getErrorMessageUnformatted(bool colored) const {
    std::string mess = std::format("{} {}{}: {}", 
        info_.type, 
        info_.numPrefix, 
        info_.num, 
        info_.mess);
    
    if (colored)
        // Red, Bold
        mess = formatColor(ANSI_RED, true, mess);

    return mess;
}

std::vector<std::string> Error::getErrorMessageLines(bool colored) const {
    if (!initialized_)
        return {};

    std::vector<std::string> lines;

    if (src_ == nullptr) {
        lines.push_back(getErrorMessageUnformatted(colored));
        return lines;
    }

    // Error message
    lines.push_back(getErrorMessageUnformatted(colored));

    // Helpers

    // The line header width is that of the biggest line
    size_t headerWidth = formatSrcLineNumber(loc_.lineEnd).size();
    // Adds the content of line 'n' 
    auto pushSrcLine = [&](int n) {
        if (n >= 1 && n <= src_->numLines()) {
            std::string nStr = std::to_string(n);
            lines.push_back(nStr + std::string(headerWidth - 1 - nStr.size(), ' ') + "| " + src_->getLine(n));
        }
    };

    // Makes the caret line for line 'n'
    auto makeCaretLine = [&](int lineNum) {
        size_t lineContentSize = src_->getLine(lineNum).size();

        size_t start = 0;
        size_t end = lineContentSize;
        bool isBeginning = false;
        if (lineNum == loc_.line) {
            start = loc_.col - 1;
            isBeginning = true;
        }

        bool isEnd = false;
        if (lineNum == loc_.lineEnd) {
            end = loc_.colEnd - 1;
            isEnd = true;
        }

        if (start > end)
            return;
    
        size_t len = end - start;

        std::string caret(headerWidth - 1, ' ');
        caret += "| ";
        caret += std::string(start, ' ');

        std::string carets;
        carets += isBeginning ? '^' : '-';

        if (len > 2)
            carets += std::string(len - 2, '-');
        
        if (len > 1)
            carets += isEnd ? '^' : '-';
        
        caret += colored ? formatColor(ANSI_RED, false, carets) : carets;
        
        lines.push_back(caret);
    };

    // Before line
    pushSrcLine(loc_.line - 1);

    // Error line(s)
    for (int i = loc_.line; i <= loc_.lineEnd; ++i) {
        pushSrcLine(i);
        makeCaretLine(i);
    }

    // After line
    pushSrcLine(loc_.lineEnd + 1);

    // Location
    {
        std::string file =
            !src_->filePath.empty() ? src_->filePath.string() :
            !src_->fileName.empty() ? src_->fileName :
            "Unknown location";

        lines.push_back(std::format("{}({},{})", file, loc_.line, loc_.col));
    }

    return lines;
}

std::string Error::getErrorMessage(bool colored) const {
    std::string res;
    for (const auto &line : getErrorMessageLines(colored))
        res += line + '\n';
    return res;
}


} // namespace NCSC
