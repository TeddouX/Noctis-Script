#pragma once
#include <stdint.h>
#include <string>

#include "token.hpp"
#include "script_node.hpp"
#include "ncsc.hpp"

namespace NCSC
{
    
class NCSC_API Error {
public:
    Error(const std::string &mess, size_t line, size_t col)
        : mess_(mess), line_(line), col_(col) {}

    Error(const std::string &mess, const Token &tok)
        : mess_(mess), line_(tok.line), col_(tok.col) {}

    Error(const std::string &mess, const ScriptNode &node);

    std::string getString();

private:
    std::string mess_;
    size_t line_ = 0;
    size_t col_ = 0;
};

} // namespace NCSC
