#include <ncsc/function.hpp>
#include <sstream>

namespace NCSC
{
 
static int64_t getInt64FromBytes(const std::vector<Byte> &bytecode, size_t offset) {
    return bytecode[offset] + 
        ((int64_t)bytecode[offset + 1] << 8) + 
        ((int64_t)bytecode[offset + 2] << 16) + 
        ((int64_t)bytecode[offset + 3] << 24) + 
        ((int64_t)bytecode[offset + 4] << 32) + 
        ((int64_t)bytecode[offset + 5] << 40) + 
        ((int64_t)bytecode[offset + 6] << 48) + 
        ((int64_t)bytecode[offset + 7] << 56);
}

static float64_t getFloat64FromBytes(const std::vector<Byte> &bytecode, size_t offset) {
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

                int16_t idx = bytecode[i++] + ((int16_t)bytecode[i++] << 8);
                oss << idx;
                break;
            }

            case static_cast<Byte>(Instruction::ADD): oss << "ADD"; break;
            case static_cast<Byte>(Instruction::SUB): oss << "SUB"; break;
            case static_cast<Byte>(Instruction::MUL): oss << "MUL"; break;
            case static_cast<Byte>(Instruction::DIV): oss << "DIV"; break;
        }
        oss << "\n";
    }

    return oss.str();
}

} // namespace NCSC
