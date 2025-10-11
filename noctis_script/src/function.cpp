#include <ncsc/function.hpp>
#include <sstream>

namespace NCSC
{
 
static constexpr int64_t getInt64FromBytes(const std::vector<Byte> &bytecode, size_t offset) {
    return bytecode[offset] + 
        ((int64_t)bytecode[offset + 1] << 8) + 
        ((int64_t)bytecode[offset + 2] << 16) + 
        ((int64_t)bytecode[offset + 3] << 24) + 
        ((int64_t)bytecode[offset + 4] << 32) + 
        ((int64_t)bytecode[offset + 5] << 40) + 
        ((int64_t)bytecode[offset + 6] << 48) + 
        ((int64_t)bytecode[offset + 7] << 56);
}

static constexpr float64_t getFloat64FromBytes(const std::vector<Byte> &bytecode, size_t offset) {
    return static_cast<float64_t>(getInt64FromBytes(bytecode, offset));
}

std::string Function::getBytecodeStrRepr() const {
    std::ostringstream oss;
    
    for (size_t i = 0; i < bytecode.size(); i++) {
        switch (bytecode[i]) {
            case static_cast<Byte>(Instruction::PUSHINT): {
                oss << "PUSHINT ";

                int64_t val = getInt64FromBytes(bytecode, i + 1);
                i += 8;
                oss << val;
                break;
            }

            case static_cast<Byte>(Instruction::PUSHFLOAT): {
                oss << "PUSHFLOAT ";

                float64_t val = getFloat64FromBytes(bytecode, i + 1);
                i += 8;
                oss << val;
                break;
            }

            case static_cast<Byte>(Instruction::STORELOCAL): {
                oss << "STORELOCAL ";

                Word idx = bytecode[i + 1] + ((Word)bytecode[i + 2] << 8);
                i += 2;
                oss << idx;
                break;
            }

            case static_cast<Byte>(Instruction::CALLSCRFUN): {
                oss << "CALLSCRFUN ";
                DWord idx = bytecode[i + 1] + ((DWord)bytecode[i + 2] << 8) + ((DWord)bytecode[i + 3] << 16) + ((DWord)bytecode[i + 4] << 24);
                i += sizeof(DWord);
                oss << idx;
                break;
            }

            case static_cast<Byte>(Instruction::ADD): oss << "ADD"; break;
            case static_cast<Byte>(Instruction::SUB): oss << "SUB"; break;
            case static_cast<Byte>(Instruction::MUL): oss << "MUL"; break;
            case static_cast<Byte>(Instruction::DIV): oss << "DIV"; break;
            case static_cast<Byte>(Instruction::RET): oss << "RET"; break;
            case static_cast<Byte>(Instruction::RETVOID): oss << "RETVOID"; break;
            case static_cast<Byte>(Instruction::NOOP): oss << "NOOP"; break;
            default: oss << "UNKNOWN INSTR"; break;
        }
        oss << "\n";
    }

    return oss.str();
}

} // namespace NCSC
