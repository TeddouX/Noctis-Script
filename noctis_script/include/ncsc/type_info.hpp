#pragma once
#include "script_node.hpp"

namespace NCSC
{

class NCSC_API TypeInfo {
public:
    TypeInfo() = default;
    TypeInfo(TokenType tokTy)
        : tokTy_(tokTy) {}

    static const TypeInfo INT;
    static const TypeInfo FLOAT;
    static const TypeInfo VOID;

    template <typename T>
    static constexpr TypeInfo fromLiteral(const T&) noexcept {
        if constexpr (std::is_integral_v<T>)
            return INT;
        else if constexpr (std::is_floating_point_v<T>)
            return FLOAT;
        else
            return VOID;
    }

    constexpr bool operator==(const TypeInfo &rhs) const noexcept {
        return tokTy_ == rhs.tokTy_;
    }

    constexpr bool operator!=(const TypeInfo& rhs) const noexcept {
        return !(*this == rhs);
    }

    bool isPrimitive() const noexcept { return isFloat() || isInt(); } 
    bool isInt() const noexcept { return tokTy_ == TokenType::INT_KWD; } 
    bool isFloat() const noexcept { return tokTy_ == TokenType::FLOAT_KWD; }
    bool isVoid() const noexcept { return tokTy_ == TokenType::FUN_KWD || tokTy_ == TokenType::INVALID; }

    void getDefaultValue(Byte* bytes, size_t size) const;

    std::string getStrRepr() const { return Token(tokTy_, "", 0, 0).getStrRepr(); }

private:
    TokenType tokTy_ = VOID.tokTy_;
};

} // namespace NCSC
