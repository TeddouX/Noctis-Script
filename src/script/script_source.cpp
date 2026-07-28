#include "script/script_source.hpp"

#include <sstream>
#include <print>


namespace NCSC
{
    
const std::string LINE_OUT_OF_RANGE_MESSAGE = "Line out of range";

ScriptSource::ScriptSource(const std::string &contents)
    : file_path{}
{
    std::istringstream contents_istream;
    contents_istream.str(contents);

    // Also count the first line
    std::size_t num_lines = std::count(contents.begin(), contents.end(), '\n') + 1; 
    lines_.reserve(num_lines);

    for (std::string line; std::getline(contents_istream, line, '\n');)
        lines_.push_back(line);

    // Last line is empty
    if (lines_.size() < num_lines)
        lines_.push_back("");
}

auto ScriptSource::from_contents(const std::string &contents) -> std::shared_ptr<ScriptSource>
{
    return std::shared_ptr<ScriptSource>(new ScriptSource{contents});
}

auto ScriptSource::get_line(std::size_t line_num) -> const std::string &
{
    if (line_num > lines_.size())
        return LINE_OUT_OF_RANGE_MESSAGE;
    return lines_[line_num - 1];
}

auto ScriptSource::get_lines() const -> const std::vector<std::string> &
{
    return lines_;
}

auto ScriptSource::get_lines_string() const -> std::string
{
    std::string output;
    for (const auto &line : lines_)
        output += line + "\n";
    return output;
}


} // namespace NCSC
