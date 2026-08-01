#pragma once
#include <memory>
#include <string>
#include <vector>
#include <filesystem>

#include "../ncsc.hpp"


namespace NCSC
{
    
class ScriptSource
{
public:
    static auto from_contents(const std::string &contents) -> PtrRef<ScriptSource>;

    // Optional: can be set to just the file name
    std::filesystem::path file_path;

    auto get_line(std::size_t line_num) -> const std::string &;
    auto get_lines() const -> const std::vector<std::string> &;
    auto get_lines_string() const -> std::string;

private:
    ScriptSource(const std::string &contents);

    std::vector<std::string> lines_;
};

} // namespace NCSC
