// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/garbage_collector.hpp>
#include <ncsc/script_context.hpp>

#include <functional>
#include <print>

namespace NCSC
{

void GarbageCollector::cleanup() {
    for (auto *obj : heap_)
        deleteObj(obj);
}

GarbageCollectorObj *GarbageCollector::allocateObj() {
    auto *obj = new GarbageCollectorObj;
    heap_.push_back(obj);
    return obj;
}

void GarbageCollector::gc(std::deque<Value> &vals) {
    if (heap_.size() <= gcTreshold_)
        return;

    gcTreshold_ = static_cast<size_t>(heap_.size() * thresholdGrowthFactor_);

    markAll(vals);
    sweep();
}

void GarbageCollector::markAll(std::deque<Value> &vals) {
    // Reset marked flag
    for (auto *obj : heap_)
        obj->marked = false;

    std::function<void (GarbageCollectorObj *)> mark = [&](GarbageCollectorObj *obj) {
        if (!obj || obj->marked) 
            return;
        
        obj->marked = true;

        for (auto *child : obj->children)
            mark(child);
    };

    // Mark every object
    for (auto &val : vals) {
        if (isObject(val.ty))
            mark(val.obj);
    }
}

void GarbageCollector::sweep() {
    for (auto *&obj : heap_) {
        if (obj->marked)
            continue;

        deleteObj(obj);
    }
}

void GarbageCollector::deleteObj(GarbageCollectorObj *&obj) {
    if (!obj) 
        return;

    if (isScriptObject(obj->type))
        delete static_cast<std::vector<Value> *>(obj->ptr);
    else {
        DWord idx = (VTypeWord)clearMask(obj->type, ValueType::CPP_OBJ_MASK);
        ctx_->destroyObject(idx, obj);
    }

    delete obj;
    obj = nullptr;
}

} // namespace NCSC
