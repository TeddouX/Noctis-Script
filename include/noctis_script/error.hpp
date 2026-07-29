#pragma once
#include <string>
#include <format>
#include <memory>
#include <vector>

#include "location.hpp"
#include "script/script_source.hpp"


namespace NCSC
{
    
enum class ErrorLevel 
{
    INFO,
    WARNING,
    ERROR,
};

auto to_string(ErrorLevel level) -> std::string_view;

struct ErrorInfo
{
public:
    std::string_view    err_name;
    std::string_view    err_code;
    std::string         err_fmt;
    ErrorLevel          err_level;

    static auto create(
        std::string_view err_name, 
        std::string_view err_code, 
        const std::string &err_fmt,
        ErrorLevel level = ErrorLevel::ERROR
    ) -> std::shared_ptr<ErrorInfo>
    {
        return std::shared_ptr<ErrorInfo>(new ErrorInfo(err_name, err_code, err_fmt, level));
    }

    template <typename... _Args>
    auto get_formatted(_Args&&... args) -> std::string
    {
        std::string formatted_msg = std::vformat(err_fmt, std::make_format_args(args...));
        return std::format("{} {} {}: {}", err_name, to_string(err_level), err_code, formatted_msg);
    }

private:
    ErrorInfo(std::string_view err_name, std::string_view err_code, const std::string &err_fmt, ErrorLevel level)
        : err_name{err_name}
        , err_code{err_code}
        , err_fmt{err_fmt}
        , err_level{level}
    {}
};

class Error
{
public:
    Error() = delete;
    Error(
        std::shared_ptr<ErrorInfo> err_info,
        const std::string &err_message, 
        std::shared_ptr<ScriptSource> script_source, 
        const Location &location = Location{}
    )
        : err_info_(err_info)
        , err_message_{err_message}
        , script_source_{script_source}
        , location_{location}
    {}

    auto get_error_message() const -> const std::string &;
    auto get_error_message_with_source() const -> std::string;

    auto get_error_info() const -> const std::shared_ptr<ErrorInfo> &;

private:
    std::shared_ptr<ErrorInfo>      err_info_;
    std::string                     err_message_;
    std::shared_ptr<ScriptSource>   script_source_;
    Location                        location_;
};

} // namespace NCSC
