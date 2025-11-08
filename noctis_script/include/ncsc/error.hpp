// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <stdint.h>
#include <string>

#include "token.hpp"
#include "script_node.hpp"
#include "script_source.hpp"
#include "ncsc.hpp"

namespace NCSC
{

struct ErrInfo {
    std::string type;
    std::string numPrefix;
    std::string mess;
    uint32_t    num;

    ErrInfo(const std::string &type, const std::string &numPrefix, uint32_t num, const std::string &mess)
        : type(type), num(num), numPrefix(numPrefix), mess(mess) {}

    // Returns a copy of this ErrInfo, formatted with the arguments
    template <typename... Args_>
    [[nodiscard]] ErrInfo format(Args_ &&...args) const {
        ErrInfo copy(*this);
        copy.mess = std::vformat(mess, std::make_format_args(args...));
        return copy;
    }
};

class NCSC_API Error {
public:
    Error(const ErrInfo &errInfo, std::shared_ptr<ScriptSource> src);

    void setLocation(size_t line, size_t col, size_t colEnd);

    const ErrInfo &getInfo() const { return info_; }
    uint32_t getLine() const { return line_; }

    std::vector<std::string> getErrorMessageLines(bool colored = true) const;
    std::string getErrorMessage(bool colored = true) const;
    std::string getErrorMessageUnformatted(bool colored = true) const;

private:
    ErrInfo info_;
    std::shared_ptr<ScriptSource> src_;

    size_t line_ = 0, col_ = 0, colEnd_ = 0;
};

} // namespace NCSC
