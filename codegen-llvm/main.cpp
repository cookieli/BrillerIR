#include <iostream>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/BasicBlock.h>
#include <vector>
#include <llvm/Support/JSON.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <fstream>
#include <sstream>
#include <memory>
using namespace std;
typedef int (*func_t)();
using IRBuilder = llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter>;
using BasicBlockFlag_T = pair<llvm::BasicBlock*, bool>;
using BasicBlockMap_T  = map<string, BasicBlockFlag_T>;
using VarToVal_T = map<string, llvm::Value*>;
using VarList_T = vector<string>;
bool isTwoArgOp(string op){
    return op == "add" || op == "sub" || op == "mul" || op == "div" ||
           op == "and" || op == "or" || op == "eq" || op == "lt" ||
           op == "gt"  || op == "le" || op == "ge";
}
bool isArithOp(string op){
    return op == "add" || op == "sub" || op =="mul" || op == "div";
}
bool isCmpOp(string op){
    return op == "lt" || op == "gt" ||
           op == "eq" || op == "le" ||
           op == "ge";
}
bool isLogicalOp(string op){
    return op == "and" || op == "or";
}
void createInst(
        IRBuilder* builder,
        llvm::json::Object* obj,
        BasicBlockMap_T* bb_map,
        VarToVal_T* val_map) {
    llvm::BasicBlock *bb = builder->GetInsertBlock();
    llvm::LLVMContext *ctx = &(builder->getContext());
    llvm::Module* module = bb->getModule();
    if (bb->getTerminator()) return;
    auto json_op = obj->getString("op");
    assert(json_op && "this instruction doesn't have op");
    auto json_dest = obj->getString("dest");
    auto json_type = obj->getString("type");
    auto json_args = obj->getArray("args");
    VarList_T args;
    args.clear();
    if (json_args) {
        for (auto ai = json_args->begin(); ai != json_args->end(); ai++) {
            if (auto json_str = ai->getAsString()) {
                string arg_str = json_str.getValue().str();
                args.push_back(arg_str);
            } else
                assert(false && "Reading argument error!\n");
        }
    }
    string op = json_op.getValue().str();
    cout << op << endl;
    llvm::Type *t_int_    = llvm::Type::getInt64Ty(*ctx);
    llvm::Type *t_bool_   = llvm::Type::getInt1Ty(*ctx);
    llvm::Type *t_char_p_ = llvm::Type::getInt8Ty(*ctx)->getPointerTo();
    if (op == "const") {
        assert(json_dest && "const instruction missing field dest!\n");
        string dest = json_dest.getValue().str();
        assert(json_type && "const instruction missing field type");
        string type = json_type.getValue().str();
        int int_val;
        bool bool_val;
        if (type == "int") {
            int_val = (obj->getInteger("value")).getValue();
            llvm::Value *alloca_val = builder->CreateAlloca(t_int_, llvm::ConstantInt::getSigned(t_int_, 1));
            (*val_map)[dest] = alloca_val;
            builder->CreateStore(llvm::ConstantInt::get(t_int_, int_val, true), alloca_val);
        } else if (type == "bool") {
            bool_val = (obj->getBoolean("value")).getValue();
            llvm::Value *alloca_val = builder->CreateAlloca(t_bool_, llvm::ConstantInt::getSigned(t_int_, 1));
            (*val_map)[dest] = alloca_val;
            builder->CreateStore(llvm::ConstantInt::get(t_bool_, bool_val), alloca_val);
        } else
            assert(false && "const instrution type does not match!\n");
    } else if (isTwoArgOp(op)) {
        assert(json_dest && "add instruction missing field dest!\n");
        string dest = json_dest.getValue().str();
        llvm::Value *lhs_ptr = (*val_map)[args[0]];
        llvm::Value *rhs_ptr = (*val_map)[args[1]];
        llvm::Value *lhs = builder->CreateLoad(lhs_ptr);
        llvm::Value *rhs = builder->CreateLoad(rhs_ptr);
        llvm::Value *dest_val;
        if (op == "add")
            dest_val = builder->CreateAdd(lhs, rhs, dest);
        else if (op == "sub")
            dest_val = builder->CreateSub(lhs, rhs, dest);
        else if (op == "mul")
            dest_val = builder->CreateMul(lhs, rhs, dest);
        else if (op == "div")
            dest_val = builder->CreateSDiv(lhs, rhs, dest);
        else if (op == "eq")
            dest_val = builder->CreateICmpEQ(lhs, rhs, dest);
        else if (op == "lt")
            dest_val = builder->CreateICmpSLT(lhs, rhs, dest);
        else if (op == "gt")
            dest_val = builder->CreateICmpSGT(lhs, rhs, dest);
        else if (op == "le")
            dest_val = builder->CreateICmpSLE(lhs, rhs, dest);
        else if (op == "ge")
            dest_val = builder->CreateICmpSGE(lhs, rhs, dest);
        else if (op == "and")
            dest_val = builder->CreateAnd(lhs, rhs, dest);
        else if (op == "or")
            dest_val = builder->CreateOr(lhs, rhs, dest);

        if (val_map->count(dest)) {
            llvm::Value *dest_ptr = (*val_map)[dest];
            builder->CreateStore(dest_val, dest_ptr);
        } else {
            llvm::Value *alloca_val;
            if (isArithOp(op))
                alloca_val = builder->CreateAlloca(t_int_,
                                                   llvm::ConstantInt::getSigned(t_int_, 1));
            else if (isCmpOp(op))
                alloca_val = builder->CreateAlloca(t_bool_, llvm::ConstantInt::getSigned(t_int_, 1));
            else if (isLogicalOp(op))
                alloca_val = builder->CreateAlloca(t_bool_, llvm::ConstantInt::getSigned(t_int_, 1));
            (*val_map)[dest] = alloca_val;
            builder->CreateStore(dest_val, alloca_val);
        }
    } else if (op == "not") {
        assert(json_dest && "not instruction missing field dest!\n");
        string dest = json_dest.getValue().str();
        llvm::Value *val_ptr = (*val_map)[args[0]];
        llvm::Value *val = builder->CreateLoad(val_ptr);
        llvm::Value *not_val = builder->CreateNot(val, dest);
        if (val_map->count(dest)) {
            llvm::Value *dest_ptr = (*val_map)[dest];
            builder->CreateStore(not_val, dest_ptr);
        } else {
            llvm::Value *alloca_val = builder->CreateAlloca(t_bool_, llvm::ConstantInt::getSigned(t_int_, 1));
            (*val_map)[dest] = alloca_val;
            builder->CreateStore(not_val, alloca_val);
        }
    }else if(op == "id"){
        assert(json_dest && "not inst missing field dest!\n");
        string dest = json_dest.getValue().str();
        llvm::Value *val_ptr = (*val_map)[args[0]];
        llvm::Value *val = builder->CreateLoad(val_ptr);
        if(val_map->count(dest)){
            llvm::Value* dest_ptr = (*val_map)[dest];
            builder->CreateStore(val, dest_ptr);
        } else {
            llvm::Type* val_type = val->getType();
            llvm::Value *alloca_val = builder->CreateAlloca(val_type, llvm::ConstantInt::getSigned(t_int_, 1));
            (*val_map)[dest] = alloca_val;
            builder->CreateStore(val, alloca_val);
        }
    } else if(op == "jmp") {
        string target = args[0];
        llvm::BasicBlock* target_bb = (*bb_map)[target].first;
        builder->CreateBr(target_bb);
        (*bb_map)[target].second = true;
    } else if(op == "br") {
        llvm::Value* cond_ptr = (*val_map)[args[0]];
        string true_target = args[1];
        string false_target = args[2];
        llvm::BasicBlock* true_target_bb = (*bb_map)[true_target].first;
        llvm::BasicBlock* false_target_bb = (*bb_map)[false_target].first;
        llvm::Value* cond = builder->CreateLoad(cond_ptr);
        builder->CreateCondBr(cond, true_target_bb, false_target_bb);
        (*bb_map)[true_target].second = true;
        (*bb_map)[true_target].second = true;
    } else if(op == "ret") {
        builder->CreateRet(llvm::ConstantInt::get(t_int_, 0 , true));
    } else if(op == "print") {
        llvm::Value* val_ptr = (*val_map)[args[0]];
        llvm::Value* val     = builder->CreateLoad(val_ptr);
        std::vector<llvm::Type*> call_types;
        call_types.push_back(t_char_p_);
        call_types.push_back(val->getType());
        llvm::FunctionType* call_ftype = llvm::FunctionType::get(t_int_, call_types, false);
        llvm::FunctionCallee printf_call = module->getOrInsertFunction("printf", call_ftype);
        std::vector<llvm::Value*> printf_args;
        printf_args.push_back(builder->CreateGlobalStringPtr("%d\n"));
        printf_args.push_back(val);
        builder->CreateCall(printf_call, printf_args);
    }
}

void createFunction(
        llvm::json::Value &json_val,
        llvm::LLVMContext & ctx,
        llvm::Module* mod,
        IRBuilder* builder
        ){
    BasicBlockMap_T bb_map;
    bb_map.clear();
    VarToVal_T  var_map;
    var_map.clear();
    llvm::Type *t_int_    = llvm::Type::getInt64Ty(ctx);
    auto json_func = json_val.getAsObject();
    assert(json_func && "json_func not existed");
    auto json_fname = json_func->getString("name");
    assert(json_fname && "function missing function name");
    string fname = json_fname.getValue().str();
    cout << fname << endl;
    std::vector<llvm::Type*> call_types;
    llvm::FunctionType* call_ftype = llvm::FunctionType::get(t_int_, call_types, false);
    llvm::Function *func = llvm::Function::Create(call_ftype, llvm::Function::ExternalLinkage, fname, mod);
    auto json_instrs = json_func->getArray("instrs");
    for(auto inst = json_instrs->begin(); inst != json_instrs->end(); ++inst){
        auto obj = inst->getAsObject();
        if(auto json_label = obj->getString("label")){
            string label = json_label.getValue().str();
            llvm::BasicBlock *bb_item = llvm::BasicBlock::Create(ctx, label, func);
            bb_map[label] = BasicBlockFlag_T (bb_item, false);
        } else if(inst == json_instrs->begin()){
            llvm::BasicBlock *bb_item = llvm::BasicBlock::Create(ctx, "entry", func);
            bb_map["entry"] = BasicBlockFlag_T(bb_item, false);
        }
    }

    for(auto inst = json_instrs->begin(); inst != json_instrs->end(); ++inst){
        auto obj = inst->getAsObject();
        if(auto json_label = obj->getString("label")){
            string label = json_label.getValue().str();
            assert(bb_map.count(label) > 0 && "there is no basic label name is this");
            llvm::BasicBlock *cur_block = bb_map[label].first;
            if(inst != json_instrs->begin()) {
                llvm::BasicBlock *pre_block = builder->GetInsertBlock();
                llvm::Instruction* ter = pre_block->getTerminator();
                if(ter == nullptr){
                    builder->CreateBr(cur_block);
                    bb_map[label].second = true;
                }
                builder->SetInsertPoint(cur_block);
            }
        } else if(inst == json_instrs->begin()){
            assert(bb_map.count("entry") > 0 && "there is not entry block");
            llvm::BasicBlock *cur_bb = bb_map["entry"].first;
            builder->SetInsertPoint(cur_bb);
            bb_map["entry"].second = true;
            createInst(builder, obj, &bb_map, &var_map);
        } else{
            createInst(builder, obj, &bb_map, &var_map);
        }
    }
    builder->CreateRet(llvm::ConstantInt::get(t_int_, 0, true));
}
int main(int argc, char **argv) {
    if(argc < 3) {
        cout << "Usage: ./bril_llvm <input_file> <output_file>" << endl;
        return -1;
    }
    ifstream json_file;
    string json_str;
    stringstream json_stream;
    json_file.open(string(argv[1]), ios::in);
    std::unique_ptr<llvm::LLVMContext> _ctx = llvm::make_unique<llvm::LLVMContext>();
    std::unique_ptr<IRBuilder> _builder;
    _builder.reset(new IRBuilder(*_ctx));
    std::unique_ptr<llvm::Module> _mod;
    _mod.reset(new llvm::Module("bril_llvm", *_ctx));
    if(json_file.is_open()) {
        json_stream << json_file.rdbuf();
        json_str = json_stream.str();
    } else {
        cout << "Input JSON file " << string(argv[1]) << " does not exitst!" << endl;
        return -2;
    }
    auto program = llvm::json::parse(json_str);
    if(auto err = program.takeError()){
        cout << "can't parse" << endl;
        return -3;
    }
    if(auto obj = program->getAsObject()){
        auto funs= obj->getArray("functions");
        for(auto cur = funs->begin(); cur != funs->end(); ++cur){
            createFunction(*cur, *_ctx, _mod.get(), _builder.get());
        }
    }
    std::error_code error;
    llvm::raw_fd_ostream dest(argv[2],error, llvm::sys::fs::F_None);
    _mod->print(dest, nullptr);
    cout << "code dump"<< endl;
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    llvm::EngineBuilder builder(std::move(_mod));
    builder.setEngineKind(llvm::EngineKind::JIT);
    llvm::ExecutionEngine* ee = builder.create();
    func_t  func = (func_t)(ee->getFunctionAddress("main"));
    (*func)();
    delete ee;
    return 0;
}
