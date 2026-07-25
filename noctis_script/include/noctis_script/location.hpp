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

    Location()
        : line{0}, line_end{0}
        , column{0}, column_end{0}
    {}

    Location(
        std::size_t line, std::size_t line_end, 
        std::size_t column, std::size_t column_end
    )
        : line{line}, line_end{line_end}
        , column{column}, column_end{column_end}
    {}
};

} // namespace NCSC
