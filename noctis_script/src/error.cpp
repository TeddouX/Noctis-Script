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

void Error::setLocation(size_t line, size_t lineEnd, size_t col, size_t colEnd) {
    line_ = line;
    lineEnd_ = lineEnd;

    col_ = col;
    colEnd_ = colEnd;
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
    assert(src_ != nullptr);

    std::vector<std::string> lines;

    // Error message
    lines.push_back(getErrorMessageUnformatted(colored));

    // Helpers

    // The line header width is that of the biggest line
    size_t headerWidth = formatSrcLineNumber(lineEnd_).size();
    // Adds the content of line 'n' 
    auto pushSrcLine = [&](int n) {
        if (n >= 1 && n <= src_->numLines()) {
            std::string nStr = std::to_string(n);
            lines.push_back(nStr + std::string(headerWidth - 1 - nStr.size(), ' ') + "| " + src_->getLine(n));
        }
    };

    // Makes the caret line for line 'n'
    auto makeCaretLine = [&](int lineNum) {
        std::string text = src_->getLine(lineNum);
        size_t len = text.size();

        size_t start = 0;
        size_t end = len;
        bool isBeginning = false;
        if (lineNum == line_) {
            start = col_ - 1;
            isBeginning = true;
        }

        bool isEnd = false;
        if (lineNum == lineEnd_) {
            end = colEnd_ - 1;
            isEnd = true;
        }

        if (start >= end)
            return;

        std::string caret(headerWidth - 1, ' ');
        caret += "| ";
        caret += std::string(start, ' ');

        std::string carets;
        carets += isBeginning ? '^' : '-';

        if (end > start + 2) {
            carets += std::string(end - start - 2, '-');
        }
        
        if (start + 1 != end)
            carets += isEnd ? '^' : '-';
        
        caret += colored ? formatColor(ANSI_RED, false, carets) : carets;
        
        lines.push_back(caret);
    };

    // Before line
    pushSrcLine(line_ - 1);

    // Error line(s)
    for (int ln = line_; ln <= lineEnd_; ++ln) {
        pushSrcLine(ln);
        makeCaretLine(ln);
    }

    // After line
    pushSrcLine(lineEnd_ + 1);

    // Location
    {
        std::string file =
            !src_->filePath.empty() ? src_->filePath.string() :
            !src_->fileName.empty() ? src_->fileName :
            "Unknown location";

        lines.push_back(std::format("{}({},{})", file, line_, col_));
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
