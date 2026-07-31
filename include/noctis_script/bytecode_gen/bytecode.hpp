#pragma once
#include <memory>

#include "../script/script_source.hpp"
#include "../ncsc.hpp"
#include "../location.hpp"


namespace NCSC
{

// Bytecode format
//
// [header]
// has_debug: bool
//
// num_globals: uint16
// globals: uint64[] (offsets into contents)
//
// num_objects: uint16
// objects: uint64[] (offsets into contents)
//
// num_functions: uint16
// functions: uint64[] (offsets into contents)
//
// contents_size: uint64
// 
// [contents]
// - global:
//      type: ValueType
//      req_stack_size: uint32
//      bytecode_size: uint64
//      bytecode: byte[]
// - object:
//      num_members: uint16_t
//      members: ValueType[]
//      methods: uint64[]
// - function:
//      Also contains methods
//      name: string (null terminated) // To be able to execute it only using its name, cached in the vm
//      num_locals: uint16
//      req_stack_size: uint32
//      bytecode_size: uint64
//      bytecode: byte[]

struct BytecodeHeader
{
    bool                        has_debug_info;
    std::vector<std::uint64_t>  globals;
    std::vector<std::uint64_t>  objects;
    std::vector<std::uint64_t>  functions;
};

class Bytecode
{
public:
    struct LocationEntry
    {
        std::size_t offset;
        Location    location;
    };

    Bytecode()
        : header_{}
        , src_{nullptr}
        , has_dbg_info_{false}
        , bytes_{}
        , location_entries_{}
    {}

    Bytecode(std::shared_ptr<ScriptSource> script_src, bool has_dbg_info = false)
        : header_{ .has_debug_info = has_dbg_info }
        , src_{script_src}
        , has_dbg_info_{has_dbg_info}
        , bytes_{}
        , location_entries_{}
    {}

    auto script_source()                    const -> const std::shared_ptr<ScriptSource> &; 
    auto has_dbg_info()                     const -> bool; 
    auto bytes()                            const -> const std::vector<byte_t> &;
    auto header()                           const -> const BytecodeHeader &;
    auto location_at(std::size_t byte_idx)  const -> Location;
    
    auto to_string() -> std::string;

private:
    friend class BytecodeGenerator;

    BytecodeHeader                  header_;

    std::shared_ptr<ScriptSource>   src_;
    bool                            has_dbg_info_;

    std::vector<byte_t>             bytes_;
    std::vector<LocationEntry>      location_entries_;
};

} // namespace NCSC
