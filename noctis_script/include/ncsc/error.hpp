// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <stdint.h>
#include <string>

#include "token.hpp"
#include "ast_node.hpp"
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
    Error(const ErrInfo &errInfo)
        : info_(errInfo) {}

    void setLocation(Location loc) { loc_ = loc; }

    const ErrInfo &getInfo() const { return info_; }
    const Location &getLocation() const { return loc_; }

    std::vector<std::string> getErrorMessageLines(bool colored = true) const;
    std::string getErrorMessage(bool colored = true) const;
    std::string getErrorMessageUnformatted(bool colored = true) const;

private:
    ErrInfo info_;
    std::shared_ptr<ScriptSource> src_;
    Location loc_;

    bool simpleError_;
    std::string simpleErrorMessage_;
};

} // namespace NCSC
