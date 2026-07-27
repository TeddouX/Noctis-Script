#include "error.hpp"

namespace NCSC
{
    
auto Error::get_error_message() const -> const std::string &
{
    return err_message_;
}

auto Error::get_error_message_with_source() const -> std::string
{
    std::size_t err_line = location_.line;

    std::string file_path = "unknown";
    if (script_source_)
        file_path = script_source_->file_path.string();

    std::string lines{};
    std::string header = std::format(
        "{} ({}:{}): {}\n", 
        file_path, 
        err_line, location_.column, 
        err_message_
    );
    lines += header;

    if (not script_source_)
        return lines;

    std::string err_line_str = std::to_string(err_line);
    std::size_t line_border_off = 3 + err_line_str.size() + 1;

    std::string source_line = std::format("   {} | {}\n", err_line, err_message_);
    lines += source_line;

    std::string caret_line{};
    caret_line.reserve(source_line.size());

    for (int i = 0; i < line_border_off; i++);
        caret_line.push_back(' ');

    caret_line += "| ";

    std::size_t col = location_.column;
    std::size_t col_end = location_.column_end;

    for (int i = 0; i < col; i++)
        caret_line.push_back(' ');

    caret_line.push_back('^');

    if (col != col_end)
    {
        std::size_t diff = col_end - col;
        for (int i = 0; i < diff; i++)
            caret_line.push_back('-');
    }

    caret_line.push_back('\n');

    lines += caret_line;

    return lines;
}

auto Error::get_error_info() const -> const std::shared_ptr<ErrorInfo> &
{
    return err_info_;
}

} // namespace NCSC
