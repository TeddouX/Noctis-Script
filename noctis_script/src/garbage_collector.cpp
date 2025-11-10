// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/garbage_collector.hpp>
#include <functional>
#include <print>

namespace NCSC
{

void GarbageCollector::cleanup() {
    for (auto *obj : heap_)
        deleteObj(obj);
}

GarbageCollectedObj *GarbageCollector::allocateObj() {
    auto *obj = new GarbageCollectedObj;
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
    std::println("Marking... (vals.size() = {})", vals.size());
    // Reset marked flag
    for (auto *obj : heap_)
        obj->marked = false;

    std::function<void (GarbageCollectedObj *)> mark = [&](GarbageCollectedObj *obj) {
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
    std::println("Sweeping...");
    for (auto *obj : heap_) {
        if (obj->marked)
            continue;

        std::println("Collecting object...");

        deleteObj(obj);
    }
}

void GarbageCollector::deleteObj(GarbageCollectedObj *obj) {
    if (obj->isScriptObj)
        delete static_cast<std::vector<Value> *>(obj->ptr);

    delete obj;
}

} // namespace NCSC
