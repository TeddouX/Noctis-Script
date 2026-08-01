#include "error.hpp"

#include <print>


namespace NCSC
{

auto to_string(ErrorLevel level) -> std::string_view
{
    switch (level)
    {
        case ErrorLevel::INFO:      return "INFO";
        case ErrorLevel::WARNING:   return "WARNING";
        case ErrorLevel::ERROR:     return "ERROR";
        default:                    return "invalid ErrorLevel";
    }
}
    
auto Error::get_error_message() const -> const std::string &
{
    return err_message_;
}

auto Error::get_error_message_with_source() const -> std::string
{
    std::size_t err_line = location_.line;

    std::string file_path = "unknown";
    if (script_source_ and not script_source_->file_path.empty())
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
    {
        std::println("Tried to print an error message with source, but no source was provided.");
        return lines;
    }

    std::string err_line_str = std::to_string(err_line);
    std::size_t line_border_off = 3 + err_line_str.size() + 1;

    std::string source_line = std::format(
        "   {} | {}\n", 
        err_line, 
        script_source_->get_line(err_line)
    );
    
    lines += source_line;

    std::string caret_line{};
    caret_line.reserve(source_line.size());

    for (int i = 0; i < line_border_off; i++)
        caret_line.push_back(' ');

    caret_line += "| ";

    std::size_t col = location_.column;
    std::size_t col_end = location_.column_end;

    for (int i = 0; i < col - 1; i++)
        caret_line.push_back(' ');

    caret_line.push_back('^');

    if (col != col_end)
    {
        std::size_t diff = col_end - col;
        for (int i = 0; i < diff; i++)
            caret_line.push_back('-');
    }

    lines += caret_line;

    return lines;
}

auto Error::get_error_info() const -> const PtrRef<ErrorInfo> &
{
    return err_info_;
}

} // namespace NCSC
