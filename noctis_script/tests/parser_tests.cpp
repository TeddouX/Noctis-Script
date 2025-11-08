// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <gtest/gtest.h>
#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>

#include <print>

using namespace NCSC;

auto parse(const std::string &code) {
    auto src = ScriptSource::fromSource(code); 
    auto tokens = Lexer(src).tokenizeAll();
    return Parser(tokens, src).parseAll();
}
