#pragma once
#include <string>
#include <vector>

#include "token.hpp"
#include "../ncsc.hpp"
#include "../script/script_source.hpp"


namespace NCSC
{
    
auto tokenize(const std::string &source) -> std::vector<Token>;
auto tokenize(const PtrRef<ScriptSource> &script_source) -> std::vector<Token>;

} // namespace NCSC

