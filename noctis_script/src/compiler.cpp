// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/compiler.hpp>
#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <ncsc/optimizer.hpp>

#include <sstream>
#include <format>
#include <iomanip>
#include <print>

namespace NCSC
{

static bool isComparisonOp(TokenType tty);

std::string Compiler::disassemble(const Bytecode& bc) {
    std::ostringstream oss;
    const std::vector<Byte> &bytes = bc.getBytes();

    for (size_t i = 0; i < bytes.size();) {
        size_t offset = i;
        Byte op = bytes[i++];

        Instruction instr = static_cast<Instruction>(op);
        auto it = INSTR_INFO.find(instr);
        if (it == INSTR_INFO.end()) {
            oss << std::setw(4) << offset << ": UNKNOWN INSTR\n";
            continue;
        }

        const auto& info = it->second;
        oss << std::setw(4) << offset << ": " << info.first;

        if (instr == Instruction::PUSH) {
            oss << " ";

            size_t size = 0;
            Value val = Value::fromBytes(bytes, i, size);
            
            oss << val.getStrRepr();
            
            i += size;
        }
        else if (instr == Instruction::CALLMETHOD || instr == Instruction::CALLCPPMETHOD) {
            oss << " ";
            
            DWord objIdx    = readWord<DWord>(bytes, i);
            DWord methodIdx = readWord<DWord>(bytes, i + sizeof(DWord));

            oss << objIdx << " " << methodIdx;

            i += 2 * sizeof(DWord);
        } 
        else if (info.second > 0) {
            oss << " ";
            switch (info.second) {
                case 2: oss << readWord<Word>(bytes, i);  i += sizeof(Word);  break;
                case 4: oss << readWord<DWord>(bytes, i); i += sizeof(DWord); break;
                case 8: oss << readWord<QWord>(bytes, i); i += sizeof(QWord); break;
                default: oss << "(invalid size)";
            }
        }

        oss << "\n";
    }

    return oss.str();
}

std::unique_ptr<Script> Compiler::compileScript(std::shared_ptr<ScriptSource> source) {
    std::vector<Token> tokens = Lexer(source).tokenizeAll();
    Parser parser(tokens, source);

    ASTNode root = parser.parseAll();
    if (parser.hasErrors()) {
        std::vector<Error> parserErrors = parser.getErrors();
        compileErrors_.insert(compileErrors_.end(), parserErrors.begin(), parserErrors.end()); 
    
        return nullptr;
    }

    return compileScript(source, root);
}

std::unique_ptr<Script> Compiler::compileScript(std::shared_ptr<ScriptSource> source, const ASTNode &root) {
    src_ = source;

    // Reset for safe measure
    clearTmpBytecode();

    auto script = std::make_unique<Script>();
    script->ctx = ctx_;
    script->src = src_;

    currScript_ = script.get();

    for (const auto &node : root.children()) {
        switch (node.type()) {
            case ASTNodeType::FUNCTION:
                enterNewScope();
                compileFunction(node);
                resetScopes();

                break;

            case ASTNodeType::VARIABLE_DECLARATION: {
                compileVariableDeclaration(node, true);

                emit(Instruction::RETVOID, nullptr);

                finalizeBc(tempCompiledBytecode_);

                GlobalVar *gv = currScript_->getGlobalVariable(currScript_->numGlobalVariables - 1); 
                gv->bytecode = tempCompiledBytecode_;
                gv->requiredStackSize = computeRequiredStackSize(tempCompiledBytecode_);

                clearTmpBytecode();

                break;
            }

            case ASTNodeType::OBJECT:
                compileObject(node);
                break;
            
            default:
                break;
        }
    }

    currScript_ = nullptr;
    return script;
}

void Compiler::enterNewScope() {
    scopes_.push_back(Scope{});
    currScope_ = &scopes_.back();
    if (scopes_.size() > 1)
        currScope_->parent = &scopes_[scopes_.size() - 2];
    nextScopeIdx_++;
}

void Compiler::exitScope() {
    nextScopeIdx_--;
    if (nextScopeIdx_ == 0)
        currScope_ = nullptr;
    else {
        currScope_ = &scopes_[nextScopeIdx_ - 1];
    }
}

void Compiler::resetScopes() {
    scopes_.clear();
    currScope_ = nullptr;
    nextScopeIdx_ = 0;
}

void Compiler::finalizeBc(Bytecode &bc) {
    if (!isDebug_)
        Optimizer::optimize(tempCompiledBytecode_);
    
    resolveJumps(bc);
}

void Compiler::createCompileError(const ErrInfo &info, const ASTNode &node) {
    Error err(info, src_);
    err.setLocation({ 
        node.location.line, 
        node.location.lineEnd, 
        node.location.col, 
        node.location.colEnd 
    });
    compileErrors_.push_back(err);
}

void Compiler::emit(Byte byte, const ASTNode *node) {
    auto &bytes = tempCompiledBytecode_.bytes_;

    if (isDebug_ && node) {
        auto &locEntries = tempCompiledBytecode_.locationEntries_;
        if (locEntries.empty() 
         || locEntries.back().loc.line < node->location.line 
         || locEntries.back().loc.col < node->location.col)
            locEntries.push_back({ bytes.size(), node->location });
    }

    tempCompiledBytecode_.bytes_.push_back(byte);
}

void Compiler::emit(Instruction instr, const ASTNode *node) {
    emit(static_cast<Byte>(instr), node);
}

void Compiler::emit(const std::vector<Byte> &bytecode, const ASTNode *node) {
    for (size_t i = 0; i < bytecode.size(); i++) 
        emit(bytecode[i], node);
}

void Compiler::emit(Word dw, const ASTNode *node) {
    std::vector<Byte> bytes {
        static_cast<Byte>(dw & 0xFF),
        static_cast<Byte>((dw >> 8) & 0xFF),
    };
    emit(bytes, node);
}

void Compiler::emit(DWord dw, const ASTNode *node) {
    std::vector<Byte> bytes {
        static_cast<Byte>(dw & 0xFF),
        static_cast<Byte>((dw >> 8) & 0xFF),
        static_cast<Byte>((dw >> 16) & 0xFF),
        static_cast<Byte>((dw >> 24) & 0xFF),
    };
    emit(bytes, node);
}

void Compiler::emit(QWord qw, const ASTNode *node) {
    std::vector<Byte> bytes;
    bytes.resize(sizeof(QWord));
    makeBytes(qw, bytes, 0);
    emit(bytes, node);
}

void Compiler::patchBytecode(size_t location, Instruction instr, const std::vector<Byte> &operandBytes) {
    std::vector<Byte> tempBytes;
    tempBytes.reserve(1 + operandBytes.size());
    tempBytes.push_back(static_cast<Byte>(instr));
    tempBytes.insert(tempBytes.end(), operandBytes.begin(), operandBytes.end());

    tempCompiledBytecode_.bytes_.insert(
        tempCompiledBytecode_.bytes_.begin() + location, 
        tempBytes.begin(), 
        tempBytes.end());
}

void Compiler::compileFunction(const ASTNode &funcDecl, bool method) {
    assert(funcDecl.type() == ASTNodeType::FUNCTION);

    const auto &firstChild = funcDecl.child(0);
    int reTyNodeIdx = 0;
    // Access modifier
    if (method && firstChild.type() == ASTNodeType::TOKEN)
        reTyNodeIdx = 1;

    if (method && funcDecl.child(reTyNodeIdx).token().val == currObject_->name)
        // Constructor don't have a return type specified
        reTyNodeIdx -= 1;

    const auto &funcDeclNameNode = funcDecl.child(reTyNodeIdx + 1);
    std::string funcDeclName = funcDeclNameNode.token().val;
    if ((method && currObject_->getMethod(funcDeclName)) || currScript_->getFunction(funcDeclName)) {
        createCompileError(SYMBOL_ALREADY_EXISTS, funcDecl.child(1));
        return;
    }

    bool isConstructor = false;
    if (method) {
        isConstructor = funcDeclName == currObject_->name;
        // if (firstChild.type() == ASTNodeType::IDENTIFIER && !isConstructor) {
        //     createCompileError(NOT_A_TYPE, funcDeclNameNode);
        //     return;
        // }

        ScriptMethod me;
        me.isPublic = firstChild.token().isValid() 
                   && firstChild.token().type == TokenType::PUBLIC_KWD;
        me.numParams = 1;

        Variable thisVar;
        thisVar.name = "this";
        thisVar.type = currObject_->type;
        
        currScope_->addLocalVar(thisVar);
        
        auto &method = currObject_->emplaceMethod(me);
        currFunction_ = &method;
    } else {
        ScriptFunction fun;
        fun.numParams = 0;

        auto &function = currScript_->emplaceFunction(fun);
        currFunction_ = &function;
    }
    
    if (isConstructor) {
        currFunction_->returnTy = currObject_->type;

        // <membInit> takes a reference to 'this'
        emit(Instruction::LOADLOCAL, nullptr);
        emit((DWord)0, nullptr); // this

        emit(Instruction::CALLMETHOD, nullptr);
        // Assuming that the last object added to the 
        // script is the one that we are compiling
        emit(currScript_->getObjectCount() - 1, nullptr);
        emit((DWord)0, nullptr); // <membInit>
    }
    else {
        ASTNode retType = funcDecl.child(reTyNodeIdx); 
        if (retType.type() == ASTNodeType::DATA_TYPE)
            currFunction_->returnTy = valueTypeFromASTNode(retType);
        else
            currFunction_->returnTy = ValueType::VOID;
    }
    
    currFunction_->name = funcDeclName;

    ASTNode paramsNode = funcDecl.child(reTyNodeIdx + 2);
    currFunction_->numParams += paramsNode.numChildren() / 2;
    currFunction_->numLocals = currFunction_->numParams;
    for (int i = 0; i < paramsNode.numChildren(); i++) {
        ValueType ty = valueTypeFromASTNode(paramsNode.child(i));
        
        // Arguments are treated as local variables
        Variable v;
        v.name = paramsNode.child(++i).token().val;
        v.type = ty;

        currScope_->addLocalVar(v);
        currFunction_->paramTypes.push_back(ty);   
    }     

    const ASTNode& statementBlock = funcDecl.lastChild();
    compileStatementBlock(statementBlock);

    currFunction_->numLocals += computeMaxLocals(&scopes_.back()) - currFunction_->numParams;

    if (isConstructor) {
        if (currScope_->hasReturned) {
            createCompileError(CONSTRUCTOR_SHOULDNT_RET, funcDeclNameNode);
            return;
        }

        emit(Instruction::LOADLOCAL, nullptr);
        emit((DWord)0, nullptr); // this

        // Return 'this'
        emit(Instruction::RET, nullptr);
    }
    // For void functions, add a return at the end if none currently exists
    else if (currFunction_->returnTy == ValueType::VOID && !currScope_->hasReturned) {
        emit(Instruction::RETVOID, nullptr);
    }
    // For non void functions, error if there is no return at the end
    else if (currFunction_->returnTy != ValueType::VOID && !currScope_->hasReturned) {
        createCompileError(FUNCTION_SHOULD_RET_VAL.format(currFunction_->name), funcDeclNameNode);
        return;
    }

    if (!hasErrors()) {
        finalizeBc(tempCompiledBytecode_);

        currFunction_->requiredStackSize = computeRequiredStackSize(tempCompiledBytecode_);
        currFunction_->bytecode = tempCompiledBytecode_;
    }
    
    clearTmpBytecode();
    currFunction_ = nullptr;
}

size_t Compiler::computeRequiredStackSize(const Bytecode &bc) {
    const std::vector<Byte> &bytes = bc.bytes_;

    size_t maxSize = 0;
    size_t currSize = 0;
    ValueType lastPushedTy = ValueType::INVALID;

    for (size_t i = 0; i < bytes.size();) {
        Instruction instr = static_cast<Instruction>(bytes[i]);

        switch (instr) {
            case Instruction::LOADLOCAL:
            case Instruction::LOADGLOBAL: 
            case Instruction::LOADMEMBER: 
            case Instruction::PUSH:
            case Instruction::NEW:
            case Instruction::CPPNEW:
            case Instruction::DUP: 
                maxSize = std::max(maxSize, ++currSize);
                break;

            case Instruction::STORELOCAL: 
            case Instruction::STOREGLOBAL: 
                currSize--;
                break;
            
            case Instruction::ADD:
            case Instruction::SUB:
            case Instruction::MUL:
            case Instruction::DIV:
            case Instruction::CMPST:
            case Instruction::CMPSE:
            case Instruction::CMPGT:
            case Instruction::CMPGE:
            case Instruction::CMPEQ:
            case Instruction::CMPNE:
            case Instruction::STOREMEMBER:
                currSize -= 2;
                break;

            case Instruction::CALLSCRFUN:
            case Instruction::CALLCPPFUN: {
                DWord idx = readWord<DWord>(bytes, i + 1);
                const Function *func = instr == Instruction::CALLSCRFUN 
                    ? static_cast<Function *>(currScript_->getFunction(idx)) 
                    : static_cast<Function *>(ctx_->getCppFunction(idx));
                
                if (!func) break;

                currSize -= func->numParams;
                currSize += func->returnTy == ValueType::VOID ? 0 : 1;

                break;
            }

            case Instruction::CALLMETHOD:
            case Instruction::CALLCPPMETHOD: {
                DWord objIdx = readWord<DWord>(bytes, i + 1);
                DWord methodIdx = readWord<DWord>(bytes, i + 1 + sizeof(DWord));

                const Function *method = nullptr;
                if (instr == Instruction::CALLMETHOD) {
                    ScriptObject *scrObj = currScript_->getObject(objIdx);
                    if (!scrObj) break;
                    method = scrObj->getMethod(methodIdx);
                }
                else {
                    CPPObject *cppObj = ctx_->getCppObject(objIdx);
                    if (!cppObj) break;
                    method = cppObj->getMethod(methodIdx);
                }

                if (!method) break;

                currSize -= method->numParams;
                currSize += method->returnTy == ValueType::VOID ? 0 : 1;

                break;
            }

            default: break;
        }

        i += getInstructionSize(bytes, i);
    }

    return maxSize;
}

size_t Compiler::computeMaxLocals(const Scope *scope) {
    if (scope == nullptr)
        return 0;
    return std::max(scope->localVariables.size(), computeMaxLocals(scope->parent));
}

void Compiler::resolveJumps(Bytecode &bc) {
    std::unordered_map<QWord, size_t> jmpLocations;
    std::vector<Byte> &bytes = bc.bytes_;

    for (QWord i = 0; i < bytes.size();) {
        Instruction instr = static_cast<Instruction>(bytes[i]);
        
        switch (instr) {
            case Instruction::JMP:
            case Instruction::JMPFALSE:
            case Instruction::JMPTRUE: {
                QWord jmpLabelNum = readWord<QWord>(bytes, i + sizeof(Instruction));
                jmpLocations.emplace(jmpLabelNum, i);

                i += sizeof(QWord) + sizeof(Instruction);
                break;
            }

            case Instruction::LABEL: {
                QWord labelNum = readWord<QWord>(bytes, i + sizeof(Instruction));
                size_t jmpIdx = jmpLocations.at(labelNum);
                size_t operandIdx = jmpIdx + sizeof(Instruction);

                std::vector<Byte> operandBytes(sizeof(QWord), 0);
                makeBytes(i, operandBytes, 0);

                // Patch the jump to point to the current idx 
                std::copy(operandBytes.begin(), operandBytes.end(), bytes.begin() + operandIdx);
                // Remove the LABEL instruction
                constexpr size_t instrSize = sizeof(Instruction) + sizeof(QWord);
                bytes.erase(bytes.begin() + i, bytes.begin() + i + instrSize);
                break;
            }

            default:
                i += getInstructionSize(bytes, i);
                break;
        }
    }
}

Compiler::SymbolSearchRes Compiler::searchSymbol(const std::string &name, Object *obj) {
    // Search for members or methods in the object
    if (obj) {
        if (auto *scrObj = dynamic_cast<ScriptObject *>(obj)) {
            DWord memberIdx = scrObj->getMemberIdx(name);
            if (memberIdx != INVALID_IDX) {
                Variable *var = scrObj->getMember(memberIdx);
                return SymbolSearchRes{
                    .var = var,
                    .idx = memberIdx,
                    .foundType = var->type,
                    .ty = SymbolSearchRes::MEMBER_VAR,
                };
            }

            DWord methodIdx = scrObj->getMethodIdx(name);
            if (methodIdx != INVALID_IDX) {
                Function *method = scrObj->getMethod(methodIdx);
                return SymbolSearchRes{
                    .fun = method,
                    .idx = methodIdx,
                    .foundType = method->returnTy,
                    .ty = SymbolSearchRes::METHOD,
                };
            }
        }
        else {
            CPPObject *cppObj = dynamic_cast<CPPObject *>(obj);

            DWord cppMemberIdx = cppObj->getMemberIdx(name);
            if (cppMemberIdx != INVALID_IDX) {
                Variable *cppMemberVar = cppObj->getMember(cppMemberIdx);
                return SymbolSearchRes{
                    .var = cppMemberVar,
                    .idx = cppMemberIdx,
                    .foundType = cppMemberVar->type,
                    .ty = SymbolSearchRes::MEMBER_VAR,
                };
            }

            DWord cppMethodIdx = cppObj->getMethodIdx(name);
            if (cppMethodIdx != INVALID_IDX) {
                Function *cppMethod = cppObj->getMethod(cppMethodIdx);
                return SymbolSearchRes{
                    .fun = cppMethod,
                    .idx = cppMethodIdx,
                    .foundType = cppMethod->returnTy,
                    .ty = SymbolSearchRes::CPP_METHOD,
                };
            }
        }

        return SymbolSearchRes{};
    }

    // Local variable
    if (currScope_) {
        DWord varIdx = currScope_->getLocalVarIdx(name);
        if (varIdx != INVALID_IDX) {
            Variable *var = currScope_->getLocalVar(varIdx); 
            return SymbolSearchRes{
                .var = var,
                .idx = varIdx,
                .foundType = var->type,
                .ty = SymbolSearchRes::LOCAL_VAR,
            };
        }
    }

    // Member variable or function
    if (currObject_) {        
        DWord memberIdx = currObject_->getMemberIdx(name);
        if (memberIdx != INVALID_IDX) {
            Variable *var = currObject_->getMember(memberIdx);
            return SymbolSearchRes{
                .var = var,
                .idx = memberIdx,
                .foundType = var->type,
                .ty = SymbolSearchRes::MEMBER_VAR,
            };
        }

        DWord methodIdx = currObject_->getMethodIdx(name);
        if (methodIdx != INVALID_IDX) {
            Function *method = currObject_->getMethod(methodIdx);
            return SymbolSearchRes{
                .fun = method,
                .idx = methodIdx,
                .foundType = method->returnTy,
                .ty = SymbolSearchRes::METHOD,
            };
        } 
    }

    // Script object or function
    if (currScript_) {
        DWord funIdx = currScript_->getFunctionIdx(name);
        if (funIdx != INVALID_IDX) {
            Function *fun = currScript_->getFunction(funIdx);
            return SymbolSearchRes{
                .fun = fun,
                .idx = funIdx,
                .foundType = fun->returnTy,
                .ty = SymbolSearchRes::FUNCTION,
            };
        }

        DWord scriptObjIdx = currScript_->getObjectIdx(name);
        if (scriptObjIdx != INVALID_IDX) {
            ScriptObject *scriptObj = currScript_->getObject(scriptObjIdx);
            return SymbolSearchRes{
                .obj = scriptObj,
                .idx = scriptObjIdx,
                .foundType = scriptObj->type,
                .ty = SymbolSearchRes::OBJECT,
            };
        }

        DWord globalVarIdx = currScript_->getGlobalVariableIdx(name);
        if (globalVarIdx != INVALID_IDX) {
            GlobalVar *globalVar = currScript_->getGlobalVariable(globalVarIdx);
            return SymbolSearchRes{
                .var = globalVar,
                .idx = globalVarIdx,
                .foundType = globalVar->type,
                .ty = SymbolSearchRes::GLOBAL_VAR,
            };
        }
    }

    DWord cppFunIdx = ctx_->getCppFunctionIdx(name);
    if (cppFunIdx != INVALID_IDX) {
        Function *cppFun = ctx_->getCppFunction(cppFunIdx); 
        return SymbolSearchRes{
            .fun = cppFun,
            .idx = cppFunIdx,
            .foundType = cppFun->returnTy,
            .ty = SymbolSearchRes::CPP_FUNCTION,
        };
    }

    DWord cppObjIdx = ctx_->getCppObjectIdx(name);
    if (cppObjIdx != INVALID_IDX) {
        Object *cppObj = ctx_->getCppObject(cppObjIdx); 
        return SymbolSearchRes{
            .obj = cppObj,
            .idx = cppObjIdx,
            .foundType = cppObj->type,
            .ty = SymbolSearchRes::CPP_OBJECT,
        };
    }
        
    return SymbolSearchRes{};
}

ValueType Compiler::valueTypeFromASTNode(const ASTNode &typeNode) {
    assert(typeNode.type() == ASTNodeType::TOKEN || typeNode.type() == ASTNodeType::DATA_TYPE);
    
    const Token &tok = typeNode.token();
    switch(tok.type) {
        case TokenType::INT8_KWD:    return ValueType::INT8;
        case TokenType::INT16_KWD:   return ValueType::INT16;
        case TokenType::INT32_KWD:   return ValueType::INT32;
        case TokenType::INT64_KWD:   return ValueType::INT64;

        case TokenType::UINT8_KWD:   return ValueType::UINT8;
        case TokenType::UINT16_KWD:  return ValueType::UINT16;
        case TokenType::UINT32_KWD:  return ValueType::UINT32;
        case TokenType::UINT64_KWD:  return ValueType::UINT64;
        
        case TokenType::FLOAT32_KWD: return ValueType::FLOAT32;
        case TokenType::FLOAT64_KWD: return ValueType::FLOAT64;
        
        case TokenType::BOOL_KWD:    return ValueType::BOOL;

        default: break;
    }

    SymbolSearchRes sres = searchSymbol(tok.val);
    if (sres.ty == SymbolSearchRes::OBJECT || sres.ty == SymbolSearchRes::CPP_OBJECT)
        return sres.foundType;

    createCompileError(NOT_A_TYPE, typeNode);

    return ValueType::INVALID;
}

void Compiler::compileObject(const ASTNode &obj) {
    assert(obj.type() == ASTNodeType::OBJECT);

    ScriptObject scriptObj;
    scriptObj.name = obj.child(0).token().val;
    scriptObj.type = makeObjectType(currScript_->getObjectCount());

    auto &scriptObjRef = currScript_->emplaceObject(scriptObj);
    // Safe as long as no other objects are added after this
    currObject_ = &scriptObjRef;

    ctx_->addTypeName(currObject_->type, currObject_->name);

    // Member init method
    ScriptMethod membInit;
    membInit.name = "<membInit>";
    membInit.returnTy = ValueType::VOID;
    // 'this'
    membInit.numLocals = 1;
    membInit.numParams = 1;
    membInit.paramTypes = { currObject_->type };

    currObject_->addMethod(membInit);   

    for (const auto &child : obj.children()) {
        switch (child.type()) {
            case ASTNodeType::VARIABLE_DECLARATION: {
                // Compile variable declaration as a member variable
                compileVariableDeclaration(child, /*global*/false, /*member*/true);

                resolveJumps(tempCompiledBytecode_);

                // Add the bytecode to the mever init method
                auto &membInitBc = currObject_->getMethod(0)->bytecode.bytes_; 
                membInitBc.insert(membInitBc.end(), tempCompiledBytecode_.bytes_.begin(), tempCompiledBytecode_.bytes_.end());
                clearTmpBytecode();

                break;
            }

            case ASTNodeType::FUNCTION:
                enterNewScope();
                compileFunction(child, true);
                
                resetScopes();

                break;

            default: break;
        }
    }

    DWord constructorIdx = currObject_->getConstructorIdx();
    if (constructorIdx == INVALID_IDX) {
        ScriptMethod defaultConstructor;
        defaultConstructor.isPublic = true;
        defaultConstructor.name = currObject_->name;
        defaultConstructor.numLocals = 0;
        // 'this'
        defaultConstructor.numParams = 1;
        defaultConstructor.paramTypes = { currObject_->type };
        // One call to <membInit>
        defaultConstructor.requiredStackSize = 1;
        defaultConstructor.returnTy = currObject_->type;

        clearTmpBytecode();

        // <membInit> takes a reference to 'this'
        emit(Instruction::LOADLOCAL, nullptr);
        emit((DWord)0, nullptr); // this

        emit(Instruction::CALLMETHOD, nullptr);
        emit((VTypeWord)clearMask(currObject_->type, ValueType::OBJ_MASK), nullptr);
        emit((DWord)0, nullptr); // <membInit>

        emit(Instruction::LOADLOCAL, nullptr);
        emit((DWord)0, nullptr); // this

        emit(Instruction::RET, nullptr);

        defaultConstructor.bytecode.bytes_ = tempCompiledBytecode_.bytes_;

        currObject_->addMethod(defaultConstructor);

        clearTmpBytecode();
    }

    // Optimize the member init method's bytecode
    ScriptMethod *membInitFinal = currObject_->getMethod(0);
    membInitFinal->bytecode.bytes_.push_back(static_cast<Byte>(Instruction::RETVOID));

    finalizeBc(membInitFinal->bytecode);
    membInitFinal->requiredStackSize = computeRequiredStackSize(membInitFinal->bytecode);
    
    currObject_ = nullptr;
}

void Compiler::compileVariableDeclaration(const ASTNode &varDecl, bool global, bool member) {
    assert(varDecl.type() == ASTNodeType::VARIABLE_DECLARATION);

    const auto &firstChild = varDecl.child(0);
    size_t varTyNodeIdx = 0;
    // Access modifier for a member variable
    if (member && firstChild.type() == ASTNodeType::TOKEN)
        varTyNodeIdx = 1;

    const ASTNode &assignmentNode = varDecl.child(varTyNodeIdx + 1); 
    const auto &varNameNode = assignmentNode.child(0).child(0);

    if (varNameNode.numChildren() > 1 || varNameNode.numChildren() == 0) {
        createCompileError(EXPECTED_AN_ID, varNameNode);
        return;
    }

    const ASTNode &varNameVal = varNameNode.child(0);
    const std::string &varName = varNameVal.token().val;

    if (searchSymbol(varName).var) {
        createCompileError(SYMBOL_ALREADY_EXISTS, varNameNode);
        return;
    }

    ValueType varType = valueTypeFromASTNode(varDecl.child(varTyNodeIdx));

    if (global) {
        GlobalVar gv;
        gv.name = varName;
        gv.type = varType;

        currScript_->addGlobalVariable(gv);
        currScript_->numGlobalVariables++;
    } else if (member) {
        Member mv;
        mv.name = varName;
        mv.type = varType;
        mv.isPublic = firstChild.token().isValid() 
                   && firstChild.token().type == TokenType::PUBLIC_KWD;

        currObject_->addMember(mv);
        currObject_->numMembers++;
    } else {
        Variable v;
        v.name = varName;
        v.type = varType;

        currScope_->addLocalVar(v);
    }

    compileAssignment(assignmentNode);
}

void Compiler::compileExpression(const ASTNode &expr, ValueType expectedType) {
    assert(expr.type() == ASTNodeType::EXPRESSION);

    if (expr.numChildren() == 1 && expr.child(0).type() == ASTNodeType::EXPRESSION_TERM) {
        compileExpressionTerm(expr.child(0), expectedType);
        return;
    }

    ASTNode rootOp = expr.child(0);
    recursivelyCompileExpression(rootOp, expectedType);
}

void Compiler::recursivelyCompileExpression(const ASTNode &exprChild, ValueType expectedType, bool shouldBeAssignable) {
    if (exprChild.type() == ASTNodeType::BINOP)
        compileBinaryOp(exprChild, expectedType);
    else if (exprChild.type() == ASTNodeType::EXPRESSION_TERM)
        compileExpressionTerm(exprChild, expectedType, shouldBeAssignable);
}

void Compiler::compileBinaryOp(const ASTNode &op, ValueType expectedType) {
    assert(op.type() == ASTNodeType::BINOP);

    TokenType opTokTy = op.token().type;
    ASTNode left = op.child(0);
    ASTNode right = op.child(1);

    if (opTokTy == TokenType::LOGICAL_AND || opTokTy == TokenType::LOGICAL_OR) {
        bool isAnd = opTokTy == TokenType::LOGICAL_AND;

        Value trueVal = Value::fromLiteral(true);
        Value falseVal = Value::fromLiteral(false);

        recursivelyCompileExpression(left, ValueType::BOOL);

        if (isAnd)
            // Stop computing the operands if one was false
            emit(Instruction::JMPFALSE, &op);
        else
            // Stop computing the operands if one was true
            emit(Instruction::JMPTRUE, &op);
        
        size_t pushLabelNum = tmpLabelNum_++;
        emit(pushLabelNum, &op);

        recursivelyCompileExpression(right, ValueType::BOOL);

        if (isAnd) emit(Instruction::JMPFALSE, &op);
        else       emit(Instruction::JMPTRUE, &op);

        size_t pushLabelNum1 = tmpLabelNum_++;
        emit(pushLabelNum1, &op);

        std::vector<Byte> tmpBytes;
        // Size is the same for true and false
        tmpBytes.resize(trueVal.getSize());
        
        if (isAnd)
            // For an logical and, if the end was reached, 
            // it means no JMPFALSEs were hit, so both operands were true
            trueVal.getBytes(tmpBytes, /*off*/0);
        else
            // Same as for logical and, but for JMPTRUEs
            falseVal.getBytes(tmpBytes, 0);
            
        emit(Instruction::PUSH, &op);
        emit(tmpBytes, &op);

        // Jump over the PUSH false
        size_t endLabelNum = tmpLabelNum_++;
        emit(Instruction::JMP, &op);
        emit(endLabelNum, &op);

        emit(Instruction::LABEL, nullptr);
        emit(pushLabelNum, nullptr);
        emit(Instruction::LABEL, nullptr);
        emit(pushLabelNum1, nullptr);

        if (isAnd) falseVal.getBytes(tmpBytes, /*off*/0);
        else       trueVal.getBytes(tmpBytes, 0);

        emit(Instruction::PUSH, &op);
        emit(tmpBytes, &op);

        emit(Instruction::LABEL, nullptr);
        emit(endLabelNum, nullptr);
    }
    else {
        // Comparison operators return a boolean
        // We are not type checking the operands for now, might want to change that in the future
        if (isComparisonOp(opTokTy)) {
            if (expectedType != ValueType::BOOL) {
                createCompileError(EXPECTED_TYPE_INSTEAD_GOT.format(
                    ctx_->getTypeName(expectedType), 
                    ctx_->getTypeName(ValueType::BOOL)), op);
                
                return;
            }

            expectedType = ValueType::VOID;
        }

        recursivelyCompileExpression(right, expectedType);
        recursivelyCompileExpression(left, expectedType);

        switch (opTokTy) {
            case TokenType::PLUS:             emit(Instruction::ADD, &op);   return;
            case TokenType::MINUS:            emit(Instruction::SUB, &op);   return;
            case TokenType::STAR:             emit(Instruction::MUL, &op);   return;
            case TokenType::SLASH: {
                if (!canPromoteType(ValueType::FLOAT64, expectedType)) {
                    createCompileError(DIV_ALWAYS_RETS_A_F64.format(ctx_->getTypeName(expectedType)), op);
                    return;
                }

                emit(Instruction::DIV, &op);
                return;
            }

            case TokenType::STRICTLY_SMALLER: emit(Instruction::CMPST, &op); return;
            case TokenType::SMALLER_EQUAL:    emit(Instruction::CMPSE, &op); return;
            case TokenType::STRICTLY_BIGGER:  emit(Instruction::CMPGT, &op); return;
            case TokenType::BIGGER_EQUAL:     emit(Instruction::CMPGE, &op); return;
            case TokenType::DOUBLE_EQUAL:     emit(Instruction::CMPEQ, &op); return;
            case TokenType::NOT_EQUAL:        emit(Instruction::CMPNE, &op); return;

            default:                          emit(Instruction::NOOP, &op);  return;
        }
    }
}

void Compiler::compileConstantPush(const ASTNode &constant, ValueType expectedType) {
    assert(constant.type() == ASTNodeType::CONSTANT);

    TokenType constTokTy = constant.token().type; 
    const std::string &constTokVal = constant.token().val;
    if (constTokTy == TokenType::FLOAT_CONSTANT) {
        switch(expectedType) {
            case ValueType::FLOAT32: {
                float32_t val = std::stof(constTokVal);
                std::vector<Byte> bytes(getValueSize(val), 0);
                makeValueBytes(std::bit_cast<uint32_t>(val), ValueType::FLOAT32, bytes, 0);
                emit(Instruction::PUSH, &constant);
                emit(bytes, &constant);
                break;
            }
            case ValueType::VOID:
            case ValueType::FLOAT64: {
                float64_t val = std::stod(constTokVal);
                std::vector<Byte> bytes(getValueSize(val), 0);
                makeValueBytes(std::bit_cast<uint64_t>(val), ValueType::FLOAT64, bytes, 0);
                emit(Instruction::PUSH, &constant);
                emit(bytes, &constant);
                break;
            }
            default:
                createCompileError(EXPECTED_TYPE_INSTEAD_GOT.format(
                    ctx_->getTypeName(expectedType), 
                    ctx_->getTypeName(ValueType::FLOAT64)), constant);
                return;
        }
    } else if (constTokTy == TokenType::INT_CONSTANT) {
        switch(expectedType) {
            case ValueType::INT8:    emitIntConstant<int8_t>(constTokVal, constant, ValueType::INT8);     break;
            case ValueType::INT16:   emitIntConstant<int16_t>(constTokVal, constant, ValueType::INT16);   break;
            // If a float is expected for the expression, we can safely emit an int
            // and the VM will be able to use it as a float
            case ValueType::FLOAT32:
            case ValueType::VOID:
            case ValueType::INT32:   emitIntConstant<int32_t>(constTokVal, constant, ValueType::INT32);   break;
            
            case ValueType::FLOAT64:
            case ValueType::INT64:   emitIntConstant<int64_t>(constTokVal, constant, ValueType::INT64);   break;

            case ValueType::UINT8:   emitIntConstant<uint8_t>(constTokVal, constant, ValueType::UINT8);   break;
            case ValueType::UINT16:  emitIntConstant<uint16_t>(constTokVal, constant, ValueType::UINT16); break;
            case ValueType::UINT32:  emitIntConstant<uint32_t>(constTokVal, constant, ValueType::UINT32); break;
            case ValueType::UINT64:  emitIntConstant<uint64_t>(constTokVal, constant, ValueType::UINT64); break;

            default: {
                createCompileError(EXPECTED_TYPE_INSTEAD_GOT.format(
                    ctx_->getTypeName(expectedType), 
                    ctx_->getTypeName(ValueType::INT32)), constant); 
            
                return;
            } 
        }
    } else if (constTokTy == TokenType::TRUE_KWD || constTokTy == TokenType::FALSE_KWD) {
        if (expectedType != ValueType::BOOL) {
            createCompileError(EXPECTED_TYPE_INSTEAD_GOT.format( 
                ctx_->getTypeName(expectedType), 
                ctx_->getTypeName(ValueType::BOOL)), constant);
            return;
        }
        
        if (constTokTy == TokenType::TRUE_KWD)
            emitIntConstant<int8_t>("1", constant, ValueType::BOOL);
        else 
            emitIntConstant<int8_t>("0", constant, ValueType::BOOL);
    }
}

void Compiler::compileExpressionTerm(const ASTNode &exprTerm, ValueType expectedTy, bool shouldBeAssignable) {
    assert(exprTerm.type() == ASTNodeType::EXPRESSION_TERM);

    // Compile expressions inside parentheses
    const auto& firstChild = exprTerm.child(0);
    if (firstChild.hasChildren() && firstChild.child(0).type() == ASTNodeType::EXPRESSION) {
        compileExpression(firstChild.child(0), expectedTy);
        return;
    }

    size_t exprValueIdx = SIZE_MAX;
    for (size_t i = 0; i < exprTerm.numChildren(); i++) {
        if (exprTerm.child(i).type() == ASTNodeType::EXPRESSION_VALUE) {
            exprValueIdx = i;
            break;
        }
    }

    if (exprValueIdx == SIZE_MAX)
        // Parsing error
        return;

    ASTNode exprValue = exprTerm.child(exprValueIdx);

    // Pre-ops and post-ops will push the required value on the stack
    // If an expression term has none (constant, ...), pushing the value is necessary
    bool hasValOnStack = false;
    bool valOnStackModifiable = true;
    ValueType lastTypeOnStack = ValueType::INVALID;

    // -------------------------
    // HELPERS FOR PRE-OPERATORS
    // -------------------------

    auto checkNotAssignableAndNumeric = [&](ValueType vtype) -> bool {
        if (shouldBeAssignable) {
            createCompileError(EXPECTED_ASSIGNABLE_TERM, exprTerm);
            return false;
        }

        if (vtype != ValueType::VOID && !isNumeric(vtype)) {
            createCompileError(EXPECTED_NUMERIC_TYPE.format(ctx_->getTypeName(vtype)), exprTerm);
            return false;
        }

        if (!valOnStackModifiable) {
            createCompileError(EXP_MODIFIABLE_VALUE, exprTerm);
            return false;
        }

        return true;
    };

    auto doPreIncDec = [&](TokenType op, const ASTNode &errNode) -> bool {
        SymbolSearchRes sres = searchSymbol(exprValue.child(0).token().val);
        if (sres.ty == SymbolSearchRes::INVALID) {
            createCompileError(NOT_A_VAR, exprValue);
            return false;
        }
        else if (!checkNotAssignableAndNumeric(sres.foundType))
            return false;

        // Get the value of the operand
        compileExpressionValue(exprValue, expectedTy, true);
        
        if (op == TokenType::PLUS_PLUS) 
            emit(Instruction::INC, &errNode);
        else
            emit(Instruction::DEC, &errNode);
        
        // Store the value back in the variable
        compileStore(exprTerm);

        if (expectedTy != ValueType::VOID)
            compileExpressionValue(exprValue, expectedTy, true);

        lastTypeOnStack = sres.foundType;

        return true;
    };

    // -------------------------
    // PRE-OPERATORS (++, --, !)
    // -------------------------

    size_t childIdx = 0;
    for (const auto &child : exprTerm.children()) {
        if (child.type() == ASTNodeType::EXPRESSION_PREOP) {
            TokenType preopTy = child.token().type;
            switch (preopTy) {
                case TokenType::PLUS_PLUS:
                    if (!doPreIncDec(preopTy, child)) return;
                    break;
                case TokenType::MINUS_MINUS:
                    if (!doPreIncDec(preopTy, child)) return;
                    break;

                case TokenType::NOT: {
                    if (expectedTy != ValueType::BOOL) {
                        createCompileError(EXPECTED_A_BOOLEAN.format(ctx_->getTypeName(expectedTy)), exprValue);
                        return;
                    }

                    compileExpressionValue(exprValue, expectedTy, false);
                    emit(Instruction::NOT, &child);

                    break;
                }

                default: break;
            }
            
            childIdx++;

            hasValOnStack = true;
            valOnStackModifiable = false;
        } else 
            break;
    }

    // Error while parsing
    if (exprTerm.child(childIdx).type() != ASTNodeType::EXPRESSION_VALUE)
        return;
    childIdx++;


    // --------------------------
    // HELPERS FOR POST-OPERATORS
    // --------------------------

    // Helper for post ++ / --
    auto doPostIncDec = [&](TokenType op, const ASTNode &errNode) -> bool {
        if (hasValOnStack && !checkNotAssignableAndNumeric(lastTypeOnStack))
            return false;
        else if (!hasValOnStack) {
            // Search for the operand
            SymbolSearchRes sres = searchSymbol(exprValue.child(0).token().val);
            if (sres.ty == SymbolSearchRes::INVALID) {
                createCompileError(NOT_A_VAR, exprValue);
                return false;
            }
            else if (!isNumeric(sres.foundType)) {
                createCompileError(EXPECTED_NUMERIC_TYPE.format(ctx_->getTypeName(sres.foundType)), exprValue);
                return false;
            }

            // Load original value of the variable
            compileExpressionValue(exprValue, ValueType::VOID, true);

            lastTypeOnStack = sres.foundType;
        }
        
        // One for the increment and store and another one left on the stack, 
        // if the caller expects a value
        if (expectedTy != ValueType::VOID)
            emit(Instruction::DUP, &errNode);

        if (op == TokenType::PLUS_PLUS) 
            emit(Instruction::INC, &errNode);
        else
            emit(Instruction::DEC, &errNode);
        
        compileStore(exprTerm);

        hasValOnStack = true;
        valOnStackModifiable = false;

        return true;
    };

    // - Checks if the last value on the stack is an object
    // - If the last value is invalid, calls compileExpressionValue(exprValue, ValueType::VOID);
    // and sets lastTypeOnStack accordingly
    auto checkLastValIsObj = [&]() -> bool {
        if (!valOnStackModifiable) {
            createCompileError(EXP_MODIFIABLE_VALUE, exprTerm);
            return false;
        }

        if (hasValOnStack && !isObject(lastTypeOnStack)) {
            createCompileError(TY_IS_NOT_AN_OBJECT.format(ctx_->getTypeName(lastTypeOnStack)), exprValue);
            return false;
        }

        if (!hasValOnStack) {
            compileExpressionValue(exprValue, ValueType::VOID, shouldBeAssignable);

            const auto& exprValueChild = exprValue.child(0);
            SymbolSearchRes sres;
            if (exprValueChild.type() == ASTNodeType::IDENTIFIER)
                sres = searchSymbol(exprValueChild.token().val);
            else if (exprValueChild.type() == ASTNodeType::FUNCTION_CALL
                    || exprValueChild.type() == ASTNodeType::CONSTRUCT_CALL)
                sres = searchSymbol(exprValueChild.child(0).token().val);
            
            if (sres.ty == SymbolSearchRes::INVALID)
                return false;

            lastTypeOnStack = sres.foundType; 
        }
        
        return lastTypeOnStack != ValueType::INVALID;
    };

    // ------------------------------------------------------
    // POST-OPERATORS (++, --, !, method call, member access)
    // ------------------------------------------------------

    for (; childIdx < exprTerm.numChildren(); childIdx++) {
        const auto &postOp = exprTerm.child(childIdx);
        const auto &postOpChild = postOp.child(0);

        // Method call
        if (postOpChild.type() == ASTNodeType::FUNCTION_CALL) {
            if (!checkLastValIsObj()) return;

            bool isScriptObj;
            DWord objIdx;
            Object *obj = getValueTypeAsObject(lastTypeOnStack, isScriptObj, objIdx);

            // Search for the member in the object
            const std::string &methodName = postOpChild.child(0).token().val;
            SymbolSearchRes sres = searchSymbol(methodName, obj);
            if (sres.ty != SymbolSearchRes::METHOD && sres.ty != SymbolSearchRes::CPP_METHOD) {
                createCompileError(NOT_A_METHOD.format(obj->name), postOp);
                return;
            }

            Method *method = dynamic_cast<Method *>(sres.fun);
            if (!method->isPublic) {
                createCompileError(INACESSIBLE_BC_NOT_PUB, postOp);
                return;
            }

            bool isLastPostOp = childIdx + 1 >= exprTerm.numChildren();
            if (isLastPostOp) {
                if (!canPromoteType(method->returnTy, expectedTy)) {
                    createCompileError(EXPECTED_TYPE_INSTEAD_GOT.format(
                        ctx_->getTypeName(expectedTy), 
                        ctx_->getTypeName(method->returnTy)), postOp);
                    
                    return;
                }

                // Objects are references
                if (shouldBeAssignable && !hasMask(method->returnTy, ValueType::OBJ_MASK)) {
                    createCompileError(EXP_MODIFIABLE_VALUE, postOp);
                    return;
                }
            }
            compileArguments(postOpChild.child(1), method, true);

            emit(isScriptObj ? Instruction::CALLMETHOD : Instruction::CALLCPPMETHOD, &postOpChild);
            emit(objIdx, &postOpChild);
            emit(sres.idx, &postOpChild);

            exprValue = postOpChild;
            lastTypeOnStack = method->returnTy;
            hasValOnStack = true;
            
            continue;
        }

        TokenType postOpTokTy = postOpChild.token().type;
        switch (postOpTokTy) {
            case TokenType::PLUS_PLUS:
                if (!doPostIncDec(TokenType::PLUS_PLUS, postOpChild)) return;
                break;
            case TokenType::MINUS_MINUS:
                if (!doPostIncDec(TokenType::MINUS_MINUS, postOpChild)) return;
                break;

            // Member access
            case TokenType::ID: {
                if (!checkLastValIsObj()) return;
                
                bool isScriptObj;
                DWord objIdx;
                Object *obj = getValueTypeAsObject(lastTypeOnStack, isScriptObj, objIdx);

                const std::string &postOpTokVal = postOpChild.token().val;
                // Search for the member in the object
                SymbolSearchRes sres = searchSymbol(postOpTokVal, obj);
                if (sres.ty != SymbolSearchRes::MEMBER_VAR) {
                    createCompileError(NOT_A_MEMBER.format(obj->name), postOp);
                    return;
                }

                Member *memberVar = dynamic_cast<Member *>(sres.var);
                if (!memberVar->isPublic) {
                    createCompileError(INACESSIBLE_BC_NOT_PUB, postOp);
                    return;
                }

                bool isLastPostOp = childIdx + 1 >= exprTerm.numChildren();
                if (isLastPostOp && !canPromoteType(memberVar->type, expectedTy)) {
                    createCompileError(EXPECTED_TYPE_INSTEAD_GOT.format(
                        ctx_->getTypeName(expectedTy), 
                        ctx_->getTypeName(memberVar->type)), postOp);
                    
                    return;
                }
                
                emit(Instruction::LOADMEMBER, &postOpChild);
                emit(sres.idx, &postOpChild);
                
                hasValOnStack = true;
                exprValue = postOpChild;
                lastTypeOnStack = memberVar->type;

                break;
            }
            
            default: break;
        }
    }

    if (!hasValOnStack)
        compileExpressionValue(exprValue, expectedTy, shouldBeAssignable);
}

void Compiler::compileStatementBlock(const ASTNode &stmtBlock) {
    assert(stmtBlock.type() == ASTNodeType::STATEMENT_BLOCK);

    for (const auto &stmt : stmtBlock.children()) {
        if (currScope_->hasReturned) break;

        switch (stmt.type()) {
            case ASTNodeType::IF_STATEMENT:
                compileIfStatement(stmt);
                break;
            case ASTNodeType::VARIABLE_DECLARATION:
                compileVariableDeclaration(stmt, false);
                break;
            case ASTNodeType::ASSIGNMENT:
                compileAssignment(stmt);
                break;
            case ASTNodeType::RETURN_STMT:
                compileReturn(stmt);
                break;
            default: 
                emit(Instruction::NOOP, &stmt);
                break;
        }
    }
}

void Compiler::compileFunctionCall(const ASTNode &funCall, ValueType expectedType) {
    assert(funCall.type() == ASTNodeType::FUNCTION_CALL);

    const auto &funNameNode = funCall.child(0);
    const std::string &funName = funNameNode.token().val;
    SymbolSearchRes sres = searchSymbol(funName);

    if (sres.ty == SymbolSearchRes::INVALID) {
        createCompileError(CANT_FIND_FUNCTION_NAMED.format(funName), funCall);
        return;
    }

    if (sres.ty != SymbolSearchRes::FUNCTION && sres.ty != SymbolSearchRes::CPP_FUNCTION) {
        createCompileError(NOT_A_FUNCTION, funNameNode);
        return;
    }

    DWord idx = sres.idx;
    const Function* fun;
    if (sres.ty == SymbolSearchRes::FUNCTION)
        fun = currScript_->getFunction(idx);
    else if (sres.ty == SymbolSearchRes::CPP_FUNCTION)
        fun = ctx_->getCppFunction(idx);

    if (clearMask(expectedType, ValueType::REF_MASK) != ValueType::VOID 
     && fun->returnTy == ValueType::VOID) {
        createCompileError(FUNCTION_HAS_VOID_RET_TY.format(funName), funNameNode);
        return;
    }

    if (!canPromoteType(fun->returnTy, expectedType)) {
        createCompileError(EXPECTED_TYPE_INSTEAD_GOT.format(
            ctx_->getTypeName(expectedType), 
            ctx_->getTypeName(fun->returnTy)), funCall);
        return;
    }

    ASTNode argsNode = funCall.child(1);
    compileArguments(argsNode, fun);
    
    if (sres.ty == SymbolSearchRes::FUNCTION)          emit(Instruction::CALLSCRFUN, &funCall);
    else if (sres.ty == SymbolSearchRes::CPP_FUNCTION) emit(Instruction::CALLCPPFUN, &funCall);
    else return;

    emit(idx, &funCall);
}

void Compiler::compileReturn(const ASTNode &ret) {
    assert(ret.type() == ASTNodeType::RETURN_STMT);

    ValueType funRetTy = currFunction_->returnTy;
    if (ret.hasChildren()) {
        ASTNode expr = ret.child(0);
        if (funRetTy == ValueType::VOID) {
            createCompileError(FUNCTION_SHOULDNT_RET_VAL.format(currFunction_->name), ret);
            return;
        }

        compileExpression(expr, funRetTy);
        emit(Instruction::RET, &ret);
    } else {
        if (funRetTy != ValueType::VOID) {
            createCompileError(FUNCTION_SHOULD_RET_VAL.format(currFunction_->name), ret);
            return;
        }

        emit(Instruction::RETVOID, &ret);
    }

    currScope_->hasReturned = true;
}

void Compiler::compileVariableAccess(const ASTNode &varAccess, ValueType expectedType) {
    assert(varAccess.type() == ASTNodeType::IDENTIFIER);

    std::string varAccessName = varAccess.token().val;
    SymbolSearchRes sres = searchSymbol(varAccessName);

    if (sres.ty == SymbolSearchRes::INVALID) {
        createCompileError(CANT_FIND_VAR_NAMED.format(varAccess.token().val), varAccess);
        return;
    }

    if (sres.ty != SymbolSearchRes::LOCAL_VAR 
     && sres.ty != SymbolSearchRes::GLOBAL_VAR 
     && sres.ty != SymbolSearchRes::MEMBER_VAR) {
        createCompileError(NOT_A_VAR, varAccess);
        return;
    }

    ValueType varType = sres.var->type;
    if (!canPromoteType(varType, clearMask(expectedType, ValueType::REF_MASK))) {
        createCompileError(EXPECTED_TYPE_INSTEAD_GOT.format(
            ctx_->getTypeName(expectedType), 
            ctx_->getTypeName(varType)), varAccess);
        return;
    }

    expectedType = clearMask(expectedType, ValueType::REF_MASK);
    if (sres.ty == SymbolSearchRes::LOCAL_VAR) {
        // Variables that are not primitives don't need to be loaded as a reference
        // because objects technically are references    
        emit(Instruction::LOADLOCAL, &varAccess);
    }
    else if (sres.ty == SymbolSearchRes::GLOBAL_VAR) {
        emit(Instruction::LOADGLOBAL, &varAccess);
    } 
    else if (sres.ty == SymbolSearchRes::MEMBER_VAR) {
        emit(Instruction::LOADLOCAL, &varAccess);
        emit((DWord)0, &varAccess); // 'this'
        
        emit(Instruction::LOADMEMBER, &varAccess);
    }
    else return;

    DWord idx = sres.idx;
    emit(idx, &varAccess);
}

bool Compiler::compileArguments(const ASTNode &argsNode, const Function *fun, bool isMethod) {
    assert(argsNode.type() == ASTNodeType::ARGUMENT_LIST);

    size_t argsNum = argsNode.numChildren();
    size_t numParams = fun->numParams;
    // Methods have the object as a first parameter which 
    // is automatically pushed by the compiler
    if (isMethod)
        numParams--;
    
    if (argsNum != numParams) {
        createCompileError(EXPECTED_NUM_ARGS_INSTEAD_GOT.format(numParams, argsNum), argsNode);
        return false;
    }

    if (argsNum > 0) {
        for (int i = 0; i < argsNum; i++)
            compileExpression(argsNode.child(i), fun->paramTypes[i]);
    }

    return true;
}

void Compiler::compileIfStatement(const ASTNode &ifStmt, int nestedCount) {
    assert(ifStmt.type() == ASTNodeType::IF_STATEMENT);

    bool hasElse = ifStmt.numChildren() > 2;

    // No expected type
    compileExpression(ifStmt.child(0), ValueType::BOOL);
    
    QWord ifLabelNum = tmpLabelNum_++; 
    emit(Instruction::JMPFALSE, &ifStmt);
    emit(ifLabelNum, &ifStmt);

    enterNewScope();
    compileStatementBlock(ifStmt.child(1));
    exitScope();

    if (hasElse) {
        emit(Instruction::JMP, &ifStmt);
        QWord elseLabelNum = tmpLabelNum_++; 
        emit(elseLabelNum, &ifStmt);

        // Make the if's JMPFALSE jumps over the else's JUMP
        emit(Instruction::LABEL, nullptr);
        emit(ifLabelNum, nullptr);
        
        const ASTNode &elseBrChild = ifStmt.child(2).child(0);
        if (elseBrChild.type() == ASTNodeType::IF_STATEMENT) {
            compileIfStatement(elseBrChild, nestedCount + 1);
        } else if (elseBrChild.type() == ASTNodeType::STATEMENT_BLOCK) {
            enterNewScope();
            compileStatementBlock(elseBrChild);
            exitScope();
        }

        emit(Instruction::LABEL, nullptr);
        emit(elseLabelNum, nullptr);
    } else {
        emit(Instruction::LABEL, nullptr);
        emit(ifLabelNum, nullptr);
    }
}

void Compiler::compileJmpBcPatch(size_t patchLoc, Instruction jmpInstr, size_t jmpLoc) {
    std::vector<Byte> operandBytes(sizeof(QWord), 0);
    makeBytes(static_cast<QWord>(jmpLoc), operandBytes, 0);
    patchBytecode(patchLoc, jmpInstr, operandBytes);
}

void Compiler::compileAssignment(const ASTNode &assignment) {
    assert(assignment.type() == ASTNodeType::ASSIGNMENT);

    if (assignment.numChildren() == 0)
        return;

    // A simple statement like a function call
    if (assignment.numChildren() == 1) {
        compileExpressionTerm(assignment.child(0), ValueType::VOID);
        return;
    }

    const ASTNode &varTerm = assignment.child(0);
    
    // Load variable value
    TokenType opTokTy = assignment.child(1).token().type;
    if (opTokTy != TokenType::EQUAL)
        compileExpressionTerm(varTerm, ValueType::VOID);
    
    ValueType varTermType = getExpressionTermType(varTerm);
    // Set the expected type to that of the loaded variable
    compileExpression(assignment.child(2), varTermType);

    switch (opTokTy) {
        case TokenType::PLUS_EQUAL:  emit(Instruction::ADD, &assignment); break;
        case TokenType::MINUS_EQUAL: emit(Instruction::SUB, &assignment); break;
        case TokenType::STAR_EQUAL:  emit(Instruction::MUL, &assignment); break;
        case TokenType::SLASH_EQUAL: emit(Instruction::DIV, &assignment); break;
        default:                                             break;
    }

    // Store back the value into the variable
    compileStore(varTerm);
}
    
void Compiler::compileExpressionValue(const ASTNode &exprVal, ValueType expectedTy, bool shouldBeAssignable) {
    assert(exprVal.type() == ASTNodeType::EXPRESSION_VALUE);

    ASTNode firstChild = exprVal.child(0);

    bool isConst = firstChild.type() == ASTNodeType::CONSTANT;
    bool isFnCall = firstChild.type() == ASTNodeType::FUNCTION_CALL;

    if ((isFnCall || isConst) && shouldBeAssignable) {
        createCompileError(EXPECTED_ASSIGNABLE_TERM, firstChild);
        return;
    }

    if (isConst)
        compileConstantPush(firstChild, expectedTy);
    else if (isFnCall)
        compileFunctionCall(firstChild, expectedTy);
    else if (firstChild.type() == ASTNodeType::IDENTIFIER)
        compileVariableAccess(firstChild, expectedTy);
    else if (firstChild.type() == ASTNodeType::CONSTRUCT_CALL)
        compileConstructCall(firstChild, expectedTy);
}

void Compiler::compileConstructCall(const ASTNode &constructCall, ValueType expectedTy) {
    assert(constructCall.type() == ASTNodeType::CONSTRUCT_CALL);

    const std::string &typeName = constructCall.child(0).token().val;
    SymbolSearchRes sres = searchSymbol(typeName);
    if (sres.ty != SymbolSearchRes::OBJECT && sres.ty != SymbolSearchRes::CPP_OBJECT) {
        createCompileError(NOT_AN_OBJ, constructCall);
        return;
    }

    if (clearMask(expectedTy, ValueType::REF_MASK) != sres.foundType) {
        createCompileError(EXPECTED_TYPE_INSTEAD_GOT.format(
            ctx_->getTypeName(sres.foundType), 
            ctx_->getTypeName(expectedTy)), constructCall);
        return;
    }

    bool isScriptObj = sres.ty == SymbolSearchRes::OBJECT;

    DWord objIdx = sres.idx;
    emit(isScriptObj ? Instruction::NEW : Instruction::CPPNEW, &constructCall);
    emit(objIdx, &constructCall);

    Object *obj = sres.obj;
    DWord constructorIdx = 0;
    const Method *constructor = nullptr;

    if (isScriptObj) {
        auto scriptObj = static_cast<ScriptObject *>(obj);

        constructorIdx = scriptObj->getConstructorIdx();
        constructor = scriptObj->getMethod(constructorIdx);
    } 
    else {
        auto cppObj = static_cast<CPPObject *>(obj);

        // The constructor is just a method with the 
        // same name as the object
        constructorIdx = cppObj->getMethodIdx(cppObj->name);
        constructor = cppObj->getMethod(constructorIdx);
    }

    if (!constructor) {
        createCompileError(NOT_A_CTOR.format(obj->name), constructCall);
        return;
    }

    bool canAccessPriv = currObject_ && currObject_->name == obj->name;
    if (!constructor->isPublic && !canAccessPriv) {
        createCompileError(INACESSIBLE_BC_NOT_PUB, constructCall);
        return;
    }

    compileArguments(constructCall.child(1), constructor, true);

    emit(isScriptObj ? Instruction::CALLMETHOD : Instruction::CALLCPPMETHOD, &constructCall);
    emit(objIdx, &constructCall);
    emit(constructorIdx, &constructCall);
}

ValueType Compiler::getExpressionTermType(const ASTNode &exprTerm) {
    assert(exprTerm.type() == ASTNodeType::EXPRESSION_TERM);

    ValueType res = ValueType::VOID;

    for (const auto &child : exprTerm.children()) {
        if (child.type() == ASTNodeType::EXPRESSION_PREOP)
            continue;

        if (child.type() == ASTNodeType::EXPRESSION_VALUE) {
            const auto &exprValue = child.child(0);
            switch (exprValue.type()) {
                case ASTNodeType::IDENTIFIER: {
                    const std::string &idTokVal = exprValue.token().val;
                    SymbolSearchRes sres = searchSymbol(idTokVal);

                    if (sres.ty != SymbolSearchRes::GLOBAL_VAR 
                     && sres.ty != SymbolSearchRes::LOCAL_VAR 
                     && sres.ty != SymbolSearchRes::MEMBER_VAR)
                        return ValueType::VOID;

                    res = sres.foundType;

                    break;
                }

                case ASTNodeType::FUNCTION_CALL: {
                    const std::string &funcName = exprValue.child(0).token().val;
                    SymbolSearchRes sres = searchSymbol(funcName);

                    if (sres.ty != SymbolSearchRes::CPP_FUNCTION 
                     && sres.ty != SymbolSearchRes::FUNCTION) 
                        return ValueType::VOID;
                    
                    res = sres.foundType;

                    break;
                }

                case ASTNodeType::CONSTANT: {
                    TokenType constTokTy = exprValue.token().type;

                    if (constTokTy == TokenType::INT_CONSTANT)
                        res = ValueType::INT32;
                    else if (constTokTy == TokenType::FLOAT_CONSTANT)
                        res = ValueType::FLOAT64;
                    
                    break;
                }

                case ASTNodeType::CONSTRUCT_CALL: {
                    const std::string &objName = exprValue.child(0).token().val;
                    SymbolSearchRes sres = searchSymbol(objName);

                    if (sres.ty != SymbolSearchRes::OBJECT)
                        return ValueType::VOID;

                    res = sres.foundType;

                    break;
                }
            }
        }
        else if (child.type() == ASTNodeType::EXPRESSION_POSTOP) {
            const auto &postOp = child.child(0);

            auto getObj = [&]() -> ScriptObject * {
                if (!hasMask(res, ValueType::OBJ_MASK))
                    return nullptr;

                DWord objIdx = (VTypeWord)clearMask(res, ValueType::OBJ_MASK);
                return currScript_->getObject(objIdx);
            };

            if (postOp.type() == ASTNodeType::IDENTIFIER) {
                ScriptObject *obj = getObj();
                if (!obj) return ValueType::VOID;

                const std::string &memberName = postOp.token().val;
                SymbolSearchRes sres = searchSymbol(memberName, obj);

                if (sres.ty != SymbolSearchRes::MEMBER_VAR)
                    return ValueType::VOID;

                res = sres.foundType;
            }
            else if (postOp.type() == ASTNodeType::FUNCTION_CALL) {
                ScriptObject *obj = getObj();
                if (!obj) return ValueType::VOID;

                const std::string &methodName = postOp.child(0).token().val;
                SymbolSearchRes sres = searchSymbol(methodName, obj);

                if (sres.ty != SymbolSearchRes::METHOD)
                    return ValueType::VOID;

                res = sres.foundType;
            }
            // Post-incs and post-decs need a reference
            else if (postOp.type() == ASTNodeType::TOKEN)
                res = clearMask(res, ValueType::REF_MASK);
        }
    }

    return res;
}

void Compiler::compileStore(const ASTNode &varNode) {
    ValueType lastTy = ValueType::INVALID;

    auto compileVariableStore = [&](const std::string &varName, bool store, const ASTNode &node) -> bool {
        SymbolSearchRes sres = searchSymbol(varName);
        if (sres.ty != SymbolSearchRes::GLOBAL_VAR 
            && sres.ty != SymbolSearchRes::LOCAL_VAR 
            && sres.ty != SymbolSearchRes::MEMBER_VAR)
            return false;
        
        if (sres.ty == SymbolSearchRes::GLOBAL_VAR) {
            emit(store ? Instruction::STOREGLOBAL : Instruction::LOADGLOBAL, &node);
        }
        else if (sres.ty == SymbolSearchRes::LOCAL_VAR) {
            emit(store ? Instruction::STORELOCAL : Instruction::LOADLOCAL, &node);
        }
        else if (sres.ty == SymbolSearchRes::MEMBER_VAR) {
            emit(Instruction::LOADLOCAL, &node);
            emit((DWord)0, &node); // 'this'

            emit(store ? Instruction::STOREMEMBER : Instruction::LOADMEMBER, &node);
        }

        emit(sres.idx, &node);

        lastTy = sres.foundType;

        return true;
    };

    if (varNode.type() == ASTNodeType::EXPRESSION_VALUE) {
        compileVariableStore(varNode.child(0).token().val, /*store*/true, varNode.child(0));
        return;
    }

    for (size_t i = 0; i < varNode.numChildren(); i++) {
        const auto &child = varNode.child(i);
        // Skip pre-ops
        if (!child.hasChildren()) 
            continue;
        
        const auto &firstChild = child.child(0);
        bool isLastChild = i + 1 >= varNode.numChildren()
            // Next post-op is a increment or a decrement
            || varNode.child(i + 1).child(0).type() == ASTNodeType::TOKEN;

        // Variable
        if (child.type() == ASTNodeType::EXPRESSION_VALUE) {
            // Compile as a store only if its the last child
            if (!compileVariableStore(firstChild.token().val, /*store*/isLastChild, child))
                return;
        }
        else if (child.type() == ASTNodeType::EXPRESSION_POSTOP) {
            if (firstChild.type() == ASTNodeType::FUNCTION_CALL) {
                if (isLastChild)
                    return;

                bool isScriptObj;
                DWord idx;
                Object *obj = getValueTypeAsObject(lastTy, isScriptObj, idx);
                if (!obj) return;

                const std::string &methodName = firstChild.child(0).token().val;
                SymbolSearchRes sres = searchSymbol(methodName, obj);
                if (sres.ty != SymbolSearchRes::METHOD && sres.ty != SymbolSearchRes::CPP_METHOD)
                    return;

                emit(isScriptObj ? Instruction::CALLMETHOD : Instruction::CALLCPPMETHOD, &firstChild);
                emit(idx, &firstChild);
                emit(sres.idx, &firstChild);
            }
            else if (firstChild.type() == ASTNodeType::IDENTIFIER) {
                bool isScriptObj;
                DWord idx;
                Object *obj = getValueTypeAsObject(lastTy, isScriptObj, idx);
                if (!obj) return;

                const std::string &memberName = firstChild.token().val;
                SymbolSearchRes sres = searchSymbol(memberName, obj);
                if (sres.ty != SymbolSearchRes::MEMBER_VAR)
                    return;

                if (isLastChild)
                    emit(Instruction::STOREMEMBER, &firstChild);
                else
                    emit(Instruction::LOADMEMBER, &firstChild);
                emit(sres.idx, &firstChild);

                lastTy = sres.foundType;
            }
        }
    }
}

Object *Compiler::getValueTypeAsObject(ValueType type, bool &isScriptObj, DWord &idx) {
    isScriptObj = isScriptObject(type);

    idx = isScriptObj 
        ? (VTypeWord)clearMask(type, ValueType::OBJ_MASK) 
        : (VTypeWord)clearMask(type, ValueType::CPP_OBJ_MASK);

    return isScriptObj 
        ? static_cast<Object *>(currScript_->getObject(idx)) 
        : static_cast<Object *>(ctx_->getCppObject(idx));
}

bool isComparisonOp(TokenType tty) {
    return tty == TokenType::STRICTLY_SMALLER
       || tty == TokenType::SMALLER_EQUAL
       || tty == TokenType::STRICTLY_BIGGER
       || tty == TokenType::BIGGER_EQUAL
       || tty == TokenType::DOUBLE_EQUAL
       || tty == TokenType::NOT_EQUAL;
}

} // namespace NCSC
