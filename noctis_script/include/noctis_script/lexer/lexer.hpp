#pragma once
#include <string>
#include <vector>

#include "token.hpp"


namespace NCSC
{
    
auto tokenize(const std::string &source) -> std::vector<Token>;

} // namespace NCSC

