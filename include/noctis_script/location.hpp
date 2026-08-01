#pragma once
#include <cstddef>


namespace NCSC
{

struct Location
{
    std::size_t line;
    std::size_t line_end;

    std::size_t column;
    std::size_t column_end;

    constexpr Location()
        : line{0zu}, line_end{0zu}
        , column{0zu}, column_end{0zu}
    {}

    constexpr Location(
        std::size_t line, std::size_t line_end, 
        std::size_t column, std::size_t column_end
    )
        : line{line}, line_end{line_end}
        , column{column}, column_end{column_end}
    {}

    constexpr auto operator==(const Location &other) const -> bool
    {
        return line == other.line       and
            line_end == other.line_end  and
            column == other.column      and
            column_end == other.column_end;
    }
};

} // namespace NCSC
