#include <ncsc/syntax_error.hpp>
#include <sstream>

namespace NCSC
{
    
std::string SyntaxError::getStrRepr() {
    std::ostringstream oss(std::ios_base::ate);

    oss << mess_ << "\n";
    oss << "Location (line:col): " << tok_->line << ":" << tok_->col << "\n";
    
    return oss.str();
}

} // namespace NCSC
