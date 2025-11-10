// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/instructions.hpp>
#include <ncsc/value.hpp>

namespace NCSC
{
    
size_t getInstructionSize(const std::vector<Byte> &bytes, size_t off) {
    Instruction instr = static_cast<Instruction>(bytes[off]);
    size_t size = sizeof(Instruction);

    if (instr == Instruction::PUSH) {
        size_t valSize = 0;
        Value::fromBytes(bytes, off + size, valSize);
        size += valSize;
    } else {
        auto it = INSTR_INFO.find(instr);
        if (it == INSTR_INFO.end()) 
            return size;

        const auto& info = it->second;
        size += info.second;
    }

    return size;
}

} // namespace NCSC
