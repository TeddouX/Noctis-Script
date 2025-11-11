// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <deque>

#include "ncsc.hpp"
#include "value.hpp"

namespace NCSC
{

struct GarbageCollectorConfig {
    // Number of allocations required for a garbage collection to start
    size_t gcStartThreshold = 10;
    float gcStartThresholhGrowthFactor = 2.f;

    // Number of garbage collections before a major one gets run
    size_t majorGCTreshold = 3;
    float majorGCThresholdGrowthFactor = 2.f;
};

struct GarbageCollectorStats {
    // Number of minor & major garbage collections 
    // (each major gc also does a minor one)
    size_t numMinorGCs = 0;
    size_t numMajorGCs = 0;
    size_t numAllocations = 0;

    void reset() { 
        numMinorGCs = 0;
        numMajorGCs = 0;
        numAllocations = 0;
    }
};

class NCSC_API GarbageCollector {
public:
    // gcStartTreshold in number of objects in the nursery (objects that are newly allocated)
    GarbageCollector(std::shared_ptr<ScriptContext> ctx, const GarbageCollectorConfig &conf)
        : ctx_(ctx), 
          conf_(conf) {}

    ~GarbageCollector() { cleanup(); }

    // Allocates an GarbageCollectorObj on the heap
    GarbageCollectorObj *allocateObj();
    void gc(std::deque<Value> &roots);

    void cleanup();

    const GarbageCollectorStats &getStats() const { return stats_; }

private:
    std::shared_ptr<ScriptContext> ctx_;

    std::vector<GarbageCollectorObj *> heap_; // All objects, used at cleanup

    std::vector<GarbageCollectorObj *> nursery_; // Newer objects
    std::vector<GarbageCollectorObj *> gen1_; // Older objects

    GarbageCollectorConfig conf_;
    GarbageCollectorStats stats_;

    void mark(GarbageCollectorObj *root);
    void sweep(std::vector<GarbageCollectorObj *> &gen);

    void minorGC(std::deque<Value> &roots);
    void majorGC(std::deque<Value> &roots);

    void deleteObj(GarbageCollectorObj *&obj);

    GarbageCollectorObj *getGCObj(const Value &val);
};

} // namespace NCSC

