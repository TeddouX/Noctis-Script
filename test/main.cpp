// The code below is not covered by the project's main license (BSD 2-Clause). 
// It can be used freely without including the license.
#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <ncsc/compiler.hpp>
#include <ncsc/script_context.hpp>
#include <ncsc/vm.hpp>
#include <print>
#include <fstream>
#include <chrono>

void printHello(int i) {
    std::println("Hello from NoctisScript! The number is: {}", i);
}

int add(int a, int b) {
    return a + b;
}

int add4(int a, int b, int c, int d) {
    return a + b + c + d;
}

class Vec2 {
public:
    Vec2() = default;
    Vec2(int x, int y) noexcept : x(x), y(y) {}

    int x;
    int y;

    int addMembers() { return x + y; }
};

int main() {
    std::ifstream ifs;

    std::stringstream buf;
    std::filesystem::path filePath = std::filesystem::absolute("test/code.ncsc");
    ifs.open(filePath);
    buf << ifs.rdbuf();

    std::string fileContents = buf.str();

    // std::println("{}", fileContents);

    auto src = NCSC::ScriptSource::fromSource(fileContents);
    src->filePath = filePath;

    auto tokens = NCSC::Lexer(src).tokenizeAll();
    NCSC::Parser parser(tokens, src);
    auto rootNode = parser.parseAll();
    // std::println("{}", rootNode.getStrRepr());
    if (parser.hasErrors()) {
        for (auto error : parser.getErrors()) 
            std::println("{}", error.getErrorMessage());

        exit(EXIT_FAILURE);
    }

    auto scriptCtx = NCSC::ScriptContext::create();
    scriptCtx->registerGlobalFunction("printHello", printHello);
    scriptCtx->registerGlobalFunction("add", add);
    scriptCtx->registerGlobalFunction("add4", add4);

    scriptCtx->registerObject<Vec2>("Vec2");
    scriptCtx->registerObjectCtor<Vec2>();
    scriptCtx->registerObjectCtor<Vec2, int, int>();
    scriptCtx->registerObjectMember("x", &Vec2::x);
    scriptCtx->registerObjectMember("y", &Vec2::y);
    scriptCtx->registerObjectMethod("AddMembers", &Vec2::addMembers);

    NCSC::Compiler compiler(scriptCtx);
    std::shared_ptr<NCSC::Script> script = compiler.compileScript(src);
    if (compiler.hasErrors()) {
        for (auto error : compiler.getErrors())
            std::println("{}", error.getErrorMessage());

        exit(EXIT_FAILURE);
    }

    for (const auto &var : script->getAllGlobalVariables()) {
        std::println("global {} {}:\n{}", 
            scriptCtx->getTypeName(var.type), 
            var.name, 
            NCSC::Compiler::disassemble(var.bytecode));
    }
    
    for (const auto &fun : script->getAllFunctions()) {
        std::string argsStr;
        for (auto ty : fun.paramTypes)
            argsStr += scriptCtx->getTypeName(ty) + ", ";

        std::println("fun {} {}({}), reqStackSize: {}, numLocals: {}:\n{}", 
            scriptCtx->getTypeName(fun.returnTy),
            fun.name,
            argsStr,
            fun.requiredStackSize,
            fun.numLocals,
            NCSC::Compiler::disassemble(fun.bytecode));
    }

    for (auto &obj : script->getAllObjects()) {
        for (const auto &method : obj.getAllMethods()) {
            std::string argsStr;
            for (auto ty : method.paramTypes)
                argsStr += scriptCtx->getTypeName(ty) + ", ";

            std::println("{} method {} {}.{}({}), reqStackSize: {}, numLocals: {}:\n{}", 
                method.isPublic ? "public" : "private", 
                scriptCtx->getTypeName(method.returnTy),
                obj.name,
                method.name,
                argsStr,
                method.requiredStackSize,
                method.numLocals,
                NCSC::Compiler::disassemble(method.bytecode));
        }
    }

    const NCSC::ScriptFunction *fun = script->getFunction("Main");
    if (fun) {
        NCSC::VM vm(script);
        if (!vm.computeGlobals()) {
            std::println("{}", vm.getLastError());
            exit(EXIT_FAILURE);
        }

        vm.prepareFunction(fun);

        if (!vm.execute()) {
            std::println("{}", vm.getLastError());
            exit(EXIT_FAILURE);
        }
        
        std::println("{}", vm.getStackStrRepr());
    }

    return 0;
}
