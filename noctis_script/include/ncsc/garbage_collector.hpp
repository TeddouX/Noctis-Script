// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <deque>

#include "ncsc.hpp"
#include "value.hpp"

namespace NCSC
{

class NCSC_API GarbageCollector {
public:
    // gcStartTreshold in number of allocations
    GarbageCollector(std::shared_ptr<ScriptContext> ctx, size_t gcStartTreshold = 10, float thresholdGrowthFactor = 1.5f)
        : ctx_(ctx), 
          gcTreshold_(gcStartTreshold), 
          thresholdGrowthFactor_(thresholdGrowthFactor) {}

    ~GarbageCollector() { cleanup(); }

    // Allocates an GarbageCollectorObj on the heap
    GarbageCollectorObj *allocateObj();
    void gc(std::deque<Value> &vals);

    void cleanup();

private:
    std::shared_ptr<ScriptContext> ctx_;
    std::vector<GarbageCollectorObj *> heap_;
    size_t gcTreshold_;
    float thresholdGrowthFactor_;

    void markAll(std::deque<Value> &vals);
    void sweep();

    void deleteObj(GarbageCollectorObj *&obj);
};

} // namespace NCSC

