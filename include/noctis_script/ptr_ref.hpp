#pragma once
#include <type_traits>

#include "types.hpp"


namespace NCSC
{

template <typename T_>
class PtrRef;

template <typename _T>
using TypeErased = PtrRef<_T>;


template <typename T_>
class PtrRef
{
public:
    using value_type = T_;

    explicit PtrRef(T_ *ptr = nullptr)
        : ptr_{ptr}
        , ref_count_{ptr ? new usize_t{1} : nullptr}
    {}

    PtrRef(std::nullptr_t) noexcept
        : ptr_{nullptr}
        , ref_count_{nullptr}
    {}

    PtrRef(const PtrRef &other)
        : ptr_{other.ptr_}
        , ref_count_{other.ref_count_}
    {
        inc_ref_count();
    }

    PtrRef(PtrRef &&other) noexcept
    {
        copy_ptrs(other);

        other.reset_ptrs();
    }

    template <typename U_>
    PtrRef(const PtrRef<U_> &other, T_ *ptr) noexcept
        : ptr_{ptr}
        , ref_count_{other.ref_count_}
    {
        inc_ref_count();
    }

    template <typename U_>
        requires(std::is_convertible_v<U_ *, T_ *>)
    PtrRef(const PtrRef<U_> &other)
        : ptr_{other.ptr_}
        , ref_count_{other.ref_count_}
    {
        inc_ref_count();
    }

    template <typename U_>
        requires(std::is_convertible_v<U_ *, T_ *>)
    explicit PtrRef(PtrRef<U_> &&other) noexcept
        : ptr_{other.ptr_}
        , ref_count_{other.ref_count_}
    {
        other.reset_ptrs();
    }

    ~PtrRef()
    {
        release();
    }

    template <typename... Args_>
        requires(std::is_constructible_v<T_, Args_...>)
    static auto make(Args_ &&...args)
    {
        return PtrRef{new T_{std::forward<Args_>(args)...}};
    }

    auto operator=(const PtrRef &other) -> PtrRef &
    {
        if (ptr_ == other.ptr_)
            return *this;

        release();
        copy_ptrs(other);

        inc_ref_count();

        return *this;
    }

    auto operator=(PtrRef &&other) noexcept -> PtrRef &
    {
        if (ptr_ == other.ptr_)
            return *this;
        
        release();
        copy_ptrs(other);
        other.reset_ptrs();

        return *this;
    }

    auto operator*() const -> T_ & 
    { 
        return *ptr_; 
    }

    auto operator->() const -> T_ *
    { 
        return ptr_; 
    }

    auto get() const -> T_ *
    {
        return ptr_;
    }

    auto use_count() const -> usize_t
    {
        if (ref_count_)
            return *ref_count_;
        
        return 0;
    }
    
    auto reset() -> void
    {
        release();
    }
    
    auto reset(T_ *ptr) -> void
    {
        release();

        ptr_ = ptr;
        ref_count_ = new usize_t{1};
    }
    
    auto swap(PtrRef &other) -> void
    {
        auto *other_ptr = other.ptr_;
        auto *other_ref_count = other.ref_count_;

        other.ptr_ = ptr_;
        other.ref_count_ = ref_count_;

        ptr_ = other_ptr;
        ref_count_ = other_ref_count;
    }

    template <typename U_>
    auto dynamic_ptr_cast() const -> PtrRef<U_>
    {
        if (auto *cast_ptr = dynamic_cast<U_ *>(get()))
            return PtrRef<U_>{*this, cast_ptr};
        return PtrRef<U_>{};
    }

    explicit operator bool() const
    {
        return ptr_ != nullptr;
    }

    friend auto operator==(const PtrRef &p, std::nullptr_t) noexcept -> bool
    {
        return p.ptr_ == nullptr;
    }

    friend auto operator!=(const PtrRef &p, std::nullptr_t) noexcept -> bool
    {
        return p.ptr_ != nullptr;
    }
    
private:
    template <typename U_>
    friend class PtrRef;

    T_         *ptr_;
    usize_t    *ref_count_;

    auto release() -> void
    {
        if (ref_count_)
        {
            (*ref_count_)--;

            if (*ref_count_ == 0)
            {
                delete ptr_;
                delete ref_count_;
            }
        }
        
        reset_ptrs();
    }

    auto copy_ptrs(const PtrRef &other) -> void
    {
        ptr_ = other.ptr_;
        ref_count_ = other.ref_count_;
    }

    auto reset_ptrs() -> void
    {
        ptr_ = nullptr;
        ref_count_ = nullptr;
    }

    auto inc_ref_count() -> void
    {
        if (ref_count_)
            (*ref_count_)++;
    }
};

} // namespace NCSC

