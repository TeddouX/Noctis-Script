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
    
    nursery_.clear();
    gen1_.clear();

    heap_.clear();
    
    stats_.reset();
}

GarbageCollectorObj *GarbageCollector::allocateObj() {
    auto *obj = new GarbageCollectorObj{};

    heap_.push_back(obj);
    nursery_.push_back(obj);

    stats_.numAllocations++;

    return obj;
}

void GarbageCollector::gc(std::deque<Value> &roots) {
    if (nursery_.empty() || nursery_.size() % conf_.gcStartThreshold != 0)
        return;

    if (stats_.numMinorGCs != 0 && stats_.numMinorGCs % conf_.majorGCTreshold == 0)
        majorGC(roots);
    else
        minorGC(roots);
}

void GarbageCollector::mark(GarbageCollectorObj *root) {
    std::vector<GarbageCollectorObj *> stack{ root };

    while(!stack.empty()) {
        // Get last inserted object on the stack for processing
        auto *obj = stack.back();
        stack.pop_back();

        if (obj->marked) continue;
        obj->marked = true;

        // Add the object's children to the stack
        // to be processed
        for (auto *objChild : obj->children) {
            if (objChild && !objChild->marked)
                stack.push_back(objChild);
        }
    }
}

void GarbageCollector::minorGC(std::deque<Value> &roots) {
    for (auto *obj : nursery_)
        obj->marked = false;

    for (const auto &root : roots) {
        if (auto obj = getGCObj(root))
            mark(obj);
    }

    sweep(nursery_);

    stats_.numMinorGCs++;
}

void GarbageCollector::majorGC(std::deque<Value> &roots) {
    for (auto *obj : heap_)
        obj->marked = false;

    for (const auto &root : roots) {
        if (auto obj = getGCObj(root))
            mark(obj);
    }

    sweep(nursery_);
    sweep(gen1_);

    stats_.numMinorGCs++;
    stats_.numMajorGCs++;
}

void GarbageCollector::sweep(std::vector<GarbageCollectorObj *> &gen) {
    gen.erase(std::remove_if(gen.begin(), gen.end(),
        [&](GarbageCollectorObj *obj) {
            if (obj == nullptr)
                return true;

            if (obj->marked) {
                // Is obj part of gen0 (nursery) ?
                if (!obj->gen) {
                    obj->gen = (bool)1;
                    // Promote it to gen1
                    gen1_.push_back(obj);
                    // Remove it from the nursery
                    return true;
                }
                // Don't do anything
                return false;
            }
            else {
                // Collect
                deleteObj(obj);
                return true;
            }
        }),
        gen.end());
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

GarbageCollectorObj *GarbageCollector::getGCObj(const Value &val) {
    if (!isObject(val.ty))
        return nullptr;
    return val.obj;
}

} // namespace NCSC
