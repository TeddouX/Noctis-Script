// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/error.hpp>
#include <format>
#include <sstream>

namespace NCSC
{

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
        mess = formatColor(31, true, mess);

    return mess;
}

std::vector<std::string> Error::getErrorMessageLines(bool colored) const {
    assert(src_ != nullptr);

   std::vector<std::string> lines;

    lines.push_back(getErrorMessageUnformatted(colored));

    auto pushSrcLine = [&](int n) {
        if (n >= 1 && n <= src_->numLines())
            lines.push_back(formatSrcLineNumber(n) + src_->getLine(n));
    };

    // Line before
    pushSrcLine(line_ - 1);
    // Error line
    pushSrcLine(line_);

    size_t lineHeaderWidth = formatSrcLineNumber(line_).size();

    // Caret line
    {
        std::string caret(lineHeaderWidth - 2, ' ');
        caret += '|';

        if (colEnd_ != 0) {
            caret += std::string(col_, ' ');
            caret += '^';
            caret += std::string(colEnd_ - col_ - 1, '-');
        }

        if (colored)
            caret = formatColor(31, false, caret);

        lines.push_back(caret);
    }

    // Line after
    pushSrcLine(line_ + 1);

    // File Location
    {
        std::string fileLocation =
            !src_->filePath.empty() ? src_->filePath.string() :
            !src_->fileName.empty() ? src_->fileName :
            "Unknown location";

        lines.push_back(std::format("{}({},{})", fileLocation, line_, col_));
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
