#include <vector>
#include <functional>
#include <print>

struct Vec2 {
    int x;
    int y;

    ~Vec2() {
        std::println("Vec2 was destroyed!");
    }
};

struct Object {
    void *ref;

    // GC
    std::vector<Object *> children;
    bool marked;
};

std::vector<Object *> objHeap;
Object *allocateObj() {
    auto *objPtr = new Object();
    objHeap.push_back(objPtr);
    return objPtr;
}

void markAll(std::vector<Object *> &vec) {
    for (auto *obj : objHeap)
        obj->marked = false;

    std::function<void (Object *)> mark = [&](Object *obj) {
        if (!obj || obj->marked) 
            return;
        
        obj->marked = true;

        for (auto *child : obj->children)
            mark(child);
    };

    std::println("Marking...");
    for (auto *obj : vec)
        mark(obj);
}

void sweep() {
    std::println("Sweeping...");
    for (auto *obj : objHeap) {
        if (obj->marked)
            continue;

        std::println("Collecting an object...");
        
        // Temp
        delete static_cast<Vec2 *>(obj->ref);
        delete obj;
    }
}

int main() {
    std::vector<Object *> fakeStack;

    Object *obj = allocateObj();
    obj->ref = new Vec2();

    fakeStack.push_back(obj);

    markAll(fakeStack);
    sweep();

    // Remove Vec2
    fakeStack.pop_back();

    markAll(fakeStack);
    sweep();

    return 0;
}
