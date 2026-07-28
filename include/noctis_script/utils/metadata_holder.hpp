#pragma once
#include <string>
#include <unordered_map>
#include <any>

namespace NCSC::Utils
{
    
class MetadataHolder 
{
public:
    template<typename T>
    void set_metadata(const std::string& key, T value) 
    {
        data_[key] = std::move(value);
    }

    template<typename T>
    T get_metadata(const std::string& key) const 
    {
        return std::any_cast<T>(data_.at(key));
    }

    bool has_metadata(const std::string& key) const 
    {
        return data_.find(key) != data_.end();
    }

private:
    std::unordered_map<std::string, std::any> data_;
};

} // namespace NCSC
