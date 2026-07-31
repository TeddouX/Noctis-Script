#include "bytecode_gen/bytecode.hpp"

#include <unordered_map>

#include "vm/vm_instructions.hpp"


namespace NCSC
{

// Instruction -> (name & operand size)
const std::unordered_map<VMInstruction, std::pair<const char *, size_t>> INSTR_INFO = 
{
    { VMInstruction::NOOP,            {"NOOP",           0} },

    { VMInstruction::PUSH,            {"PUSH",           sizeof(dword_t)} },
    { VMInstruction::POP,             {"POP",            0} },
    { VMInstruction::DUP,             {"DUP",            0} },
    
    { VMInstruction::LOADLOCAL,       {"LOADLOCAL",      sizeof(dword_t)} },
    { VMInstruction::STORELOCAL,      {"STORELOCAL",     sizeof(dword_t)} },

    { VMInstruction::LOADGLOBAL,      {"LOADGLOBAL",     sizeof(dword_t)} },
    { VMInstruction::STOREGLOBAL,     {"STOREGLOBAL",    sizeof(dword_t)} },

    { VMInstruction::LOADMEMBER,      {"LOADMEMBER",     sizeof(dword_t)} },
    { VMInstruction::STOREMEMBER,     {"STOREMEMBER",    sizeof(dword_t)} },
    
    { VMInstruction::ADD,             {"ADD",            0} },
    { VMInstruction::SUB,             {"SUB",            0} },
    { VMInstruction::MUL,             {"MUL",            0} },
    { VMInstruction::DIV,             {"DIV",            0} },
    { VMInstruction::INC,             {"INC",            0} },
    { VMInstruction::DEC,             {"DEC",            0} },
    { VMInstruction::NOT,             {"NOT",            0} },

    { VMInstruction::CMPST,           {"CMPST",          0} },
    { VMInstruction::CMPSE,           {"CMPSE",          0} },
    { VMInstruction::CMPGT,           {"CMPGT",          0} },
    { VMInstruction::CMPGE,           {"CMPGE",          0} },
    { VMInstruction::CMPEQ,           {"CMPEQ",          0} },
    { VMInstruction::CMPNE,           {"CMPNE",          0} },

    { VMInstruction::JMP,             {"JMP",            sizeof(qword_t)} },
    { VMInstruction::JMPFALSE,        {"JMPFALSE",       sizeof(qword_t)} },
    { VMInstruction::JMPTRUE,         {"JMPTRUE",        sizeof(qword_t)} },

    { VMInstruction::RET,             {"RET",            0} },
    { VMInstruction::RETVOID,         {"RETVOID",        0} },

    { VMInstruction::CALLFUNC,        {"CALLFUNC",       sizeof(dword_t)} },

    { VMInstruction::NEW,             {"NEW",            sizeof(dword_t)} },

    { VMInstruction::LABEL,           {"LABEL",          sizeof(qword_t)} },
};

auto Bytecode::script_source() const -> const std::shared_ptr<ScriptSource> &
{
    return src_;
}

auto Bytecode::has_dbg_info() const -> bool
{
    return has_dbg_info_;
}

auto Bytecode::bytes() const -> const std::vector<byte_t> &
{
    return bytes_;
}

auto Bytecode::header() const -> const BytecodeHeader &
{
    return header_;
}

auto Bytecode::location_at(std::size_t byte_idx) const -> Location
{
    if (location_entries_.empty())
        return {};

    // Get first offset that is bigger that byte_idx
    auto it = std::upper_bound(
        location_entries_.begin(),
        location_entries_.end(),
        byte_idx,
        [](std::size_t value, const LocationEntry &entry) 
        {
            return value < entry.offset;
        }
    );

    if (it == location_entries_.end())
        return location_entries_.back().location;
    else if (it == location_entries_.begin())
        return it->location;
    else
        return std::prev(it)->location;
}

auto Bytecode::to_string() -> std::string
{
    return "";
}


} // namespace NCSC
