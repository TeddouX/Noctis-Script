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

    // The actual error message
    lines.push_back(getErrorMessageUnformatted(colored));


    // The line before the line where the error is located
    {
        std::string lineBefore;
        if (line_ > 1) {
            lineBefore = formatSrcLineNumber(line_ - 1) + src_->getLine(line_ - 1);
        }

        if (!lineBefore.empty())
            lines.push_back(lineBefore);
    }


    // The line where the error is located
    {
        std::string line;
        line = formatSrcLineNumber(line_) + src_->getLine(line_);

        lines.push_back(line);
    }


    size_t lineHeaderSize = formatSrcLineNumber(0).size();
    
    // Int blabla = a(1,);
    //                ^--
    {
        std::stringstream caretSstream;
        caretSstream << std::setw(lineHeaderSize - 1);
        caretSstream << "|";
        if (colEnd_ != 0) {
            caretSstream << std::setw(col_ + 1);
            caretSstream << "^";
            for (size_t i = 0; i < colEnd_ - col_ - 1; i++)
                caretSstream << '-'; 
        }

        std::string caret = caretSstream.str();
        if (colored)
            // Red, not bold
            caret = formatColor(31, false, caret);
        
        lines.push_back(caret); 
    }
    

    // The line after the line where the error is located
    {
        std::string lineAfter;
        if (line_ <= src_->numLines()) {
            lineAfter = formatSrcLineNumber(line_ + 1) + src_->getLine(line_ + 1);
        }

        if (!lineAfter.empty())
            lines.push_back(lineAfter);
    }

    
    {
        constexpr std::string_view locationFormat = "{}({},{})";
        std::string locStr = "Unknown location";
        if (!src_->filePath.empty())
            locStr = std::format(locationFormat, src_->filePath.string(), line_, col_);
        else if (!src_->fileName.empty())
            locStr = std::format(locationFormat, src_->fileName, line_, col_);
        
        lines.push_back(locStr);
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
