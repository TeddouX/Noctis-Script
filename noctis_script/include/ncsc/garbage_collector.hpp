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
    GarbageCollector(size_t gcStartTreshold = 10, float thresholdGrowthFactor = 1.5f)
        : gcTreshold_(gcStartTreshold), thresholdGrowthFactor_(thresholdGrowthFactor) {}

    ~GarbageCollector() { cleanup(); }

    // Allocates an GarbageCollectedObj on the heap
    GarbageCollectedObj *allocateObj();
    void gc(std::deque<Value> &vals);

    void cleanup();

private:
    std::vector<GarbageCollectedObj *> heap_;
    size_t gcTreshold_;
    float thresholdGrowthFactor_;

    void markAll(std::deque<Value> &vals);
    void sweep();

    void deleteObj(GarbageCollectedObj *obj);
};

} // namespace NCSC

