#pragma once
#include "script_node.hpp"

namespace NCSC
{

class NCSC_API TypeInfo {
public:
    TypeInfo() = default;
    TypeInfo(TokenType tokTy)
        : tokTy_(tokTy) {}

    static const TypeInfo INT16;
    static const TypeInfo INT32;
    static const TypeInfo INT64;
    static const TypeInfo UINT16;
    static const TypeInfo UINT32;
    static const TypeInfo UINT64;
    static const TypeInfo FLOAT32;
    static const TypeInfo FLOAT64;
    static const TypeInfo BOOL;
    static const TypeInfo VOID;

    template <typename T>
    static TypeInfo fromLiteral(const T&) noexcept {
        if constexpr (std::is_same_v<T, int16_t>) return INT16;
        else if constexpr (std::is_same_v<T, int32_t>) return INT32;
        else if constexpr (std::is_same_v<T, int64_t>) return INT64;
        else if constexpr (std::is_same_v<T, uint16_t>) return UINT16;
        else if constexpr (std::is_same_v<T, uint32_t>) return UINT16;
        else if constexpr (std::is_same_v<T, uint64_t>) return UINT16;
        else if constexpr (std::is_same_v<T, float32_t>) return FLOAT32;
        else if constexpr (std::is_same_v<T, float64_t>) return FLOAT64;
        else if constexpr (std::is_same_v<T, bool>) return BOOL;
        else return VOID;
    }

    constexpr bool operator==(const TypeInfo &rhs) const noexcept {
        return tokTy_ == rhs.tokTy_;
    }

    constexpr bool operator!=(const TypeInfo& rhs) const noexcept {
        return !(*this == rhs);
    }

    bool isPrimitive() const noexcept { return isFloat() || isInt(); } 
    bool isInt() const noexcept { return tokTy_ == TokenType::INT16_KWD || tokTy_ == TokenType::INT32_KWD || tokTy_ == TokenType::INT64_KWD
                                    || tokTy_ == TokenType::UINT16_KWD || tokTy_ == TokenType::UINT32_KWD || tokTy_ == TokenType::UINT64_KWD; } 
    bool isFloat() const noexcept { return tokTy_ == TokenType::FLOAT32_KWD || tokTy_ == TokenType::FLOAT64_KWD; }
    bool isVoid() const noexcept { return tokTy_ == TokenType::FUN_KWD || tokTy_ == TokenType::INVALID; }

    void getDefaultValue(Byte* bytes, size_t size) const;

    std::string getStrRepr() const { return Token(tokTy_, "", 0, 0).getStrRepr(); }

private:
    TokenType tokTy_ = VOID.tokTy_;
};

} // namespace NCSC
