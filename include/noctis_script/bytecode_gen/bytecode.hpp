#pragma once
#include <memory>

#include "../script/script_source.hpp"
#include "../ncsc.hpp"
#include "../location.hpp"


namespace NCSC
{
    
class Bytecode
{
public:
    struct LocationEntry
    {
        std::size_t offset;
        Location    location;
    };

    Bytecode()
        : src_{nullptr}
        , has_dbg_info_{false}
        , bytes_{}
        , location_entries_{}
    {}

    Bytecode(std::shared_ptr<ScriptSource> script_src, bool has_dbg_info = false)
        : src_{script_src}
        , has_dbg_info_{has_dbg_info}
        , bytes_{}
        , location_entries_{}
    {}

    auto script_source()                    const -> const std::shared_ptr<ScriptSource> &; 
    auto has_dbg_info()                     const -> bool; 
    auto bytes()                            const -> const std::vector<byte_t> &;
    auto location_at(std::size_t byte_idx)  const -> Location; 

private:
    friend class Compiler;

    std::shared_ptr<ScriptSource>   src_;
    bool                            has_dbg_info_;

    std::vector<byte_t>             bytes_;
    std::vector<LocationEntry>      location_entries_;
};

} // namespace NCSC
