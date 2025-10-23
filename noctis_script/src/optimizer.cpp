// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/optimizer.hpp>
#include <ncsc/value.hpp>
#include <print>

namespace NCSC
{

std::vector<Byte> Optimizer::optimizeAll() {
    auto res = bc_;
    bool changed = true;

    while (changed) {
        changed = false;
        
        for (size_t i = 0; i < res.size();) {
            size_t oldInstrIdx = i;
            
            for (auto &optimizationRule : rules_) {
                if (optimizationRule.rule(res, i)) {
                    changed = true;
                    // Restart from the beginning
                    break;
                }
            }

            if (changed)
                break;
            else
                i += getInstructionSize(res, i);
        }
    }

    return res;
}

static constexpr Instruction toInstruction(const Byte &byte) {
    return static_cast<Instruction>(byte);
}

static bool isBinOp(Instruction &instr) {
    return instr == Instruction::ADD
        || instr == Instruction::SUB
        || instr == Instruction::MUL
        || instr == Instruction::DIV
        || instr == Instruction::CMPST
        || instr == Instruction::CMPSE
        || instr == Instruction::CMPGT
        || instr == Instruction::CMPGE
        || instr == Instruction::CMPEQ
        || instr == Instruction::CMPNE;
}

#define TRY_READ_INSTR_COND(varName, cond)  \
    auto varName = toInstruction(bc[end]);  \
    if (!cond)                              \
        return false;                       \
    end += sizeof(Instruction)

#define TRY_READ_INSTR(varName, instr) TRY_READ_INSTR_COND(varName, (varName == instr))

bool Optimizer::constantFolding(std::vector<Byte> &bc, size_t &idx) {
    size_t begin = idx;
    size_t end = begin;
    
    TRY_READ_INSTR(first, Instruction::PUSH);

    size_t readSize = 0;
    Value b = Value::fromBytes(bc, end, readSize);
    if (!isPrimitive(b.type()))
        return false;
    end += readSize;

    TRY_READ_INSTR(second, Instruction::PUSH);

    readSize = 0;
    Value a = Value::fromBytes(bc, end, readSize);
    if (!isPrimitive(a.type()))
        return false;
    end += readSize;

    TRY_READ_INSTR_COND(op, isBinOp(op));

    Value res;
    switch (op) {
        // Regular binops
        case Instruction::ADD:   res = a + b;  break;
        case Instruction::SUB:   res = a -  b; break;
        case Instruction::MUL:   res = a *  b; break;
        case Instruction::DIV:   res = a /  b; break;
        // Comparison
        case Instruction::CMPST: res = a <  b; break;
        case Instruction::CMPSE: res = a <= b; break;
        case Instruction::CMPGT: res = a >  b; break;
        case Instruction::CMPGE: res = a >= b; break;
        case Instruction::CMPEQ: res = a != b; break;
        case Instruction::CMPNE: res = a == b; break;
    }

    std::vector<Byte> bytes;
    bytes.resize(sizeof(Instruction) + res.getSize());
    bytes[0] = static_cast<Byte>(Instruction::PUSH);
    res.getBytes(bytes, sizeof(Instruction));

    bc.insert(bc.begin() + end, bytes.begin(), bytes.end());
    bc.erase(bc.begin() + begin, bc.begin() + end);

    idx += end - begin;

    return true;
}

bool Optimizer::collapseLoadPopSetObjToStore(std::vector<Byte> &bc, size_t &idx) {
    size_t begin = idx;
    size_t end = begin;

    bool global;
    auto a = toInstruction(bc[idx]); 
    if (a == Instruction::LOADGLOBAL)
        global = true;
    else if (a == Instruction::LOADLOCAL)
        global = false;
    else
        return false;

    end += sizeof(Instruction);
    DWord idxOperand = readWord<DWord>(bc, end);
    end += sizeof(DWord);

    TRY_READ_INSTR(b, Instruction::POP);
    TRY_READ_INSTR(c, Instruction::SETOBJ);

    std::vector<Byte> bytes;
    bytes.resize(sizeof(Instruction) + sizeof(DWord));
    bytes[0] = static_cast<Byte>(global ? Instruction::STOREGLOBAL : Instruction::STORELOCAL);
    makeBytes(idxOperand, bytes, sizeof(Instruction));

    bc.insert(bc.begin() + end, bytes.begin(), bytes.end());
    bc.erase(bc.begin() + begin, bc.begin() + end);

    idx += end - begin;

    return true;
}

} // namespace NCSC
