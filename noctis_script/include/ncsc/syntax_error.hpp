#include <stdint.h>

#include "token.hpp"
#include "ncsc.hpp"

namespace NCSC
{
    
class NCSC_API SyntaxError {
public:
    SyntaxError(const Token *tok, const char *mess)
        : tok_(tok), mess_(mess) {}

    std::string getStrRepr();

private:
    const Token *tok_;
    const char *mess_;
};

} // namespace NCSC
