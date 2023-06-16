
#include "default-defs.h"
#include <list>
#include <map>
#include <ostream>
#include <iostream>
#include <sstream>

#ifndef YYTOKENTYPE
#include "decafcomp.tab.h"
#endif

using namespace std;
using namespace llvm;
//forward declaration

static Module *TheModule;

// this is the method used to construct the LLVM intermediate code (IR)
static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);

/// decafAST - Base class for all abstract syntax tree nodes.

class methodArgAST;
class methodArgList;
class typedSymbol;
class typedSymbolList;
class Statement;
class StatementList;
class methodCallAST{
    string name;
    methodArgList *mlist;
public:
    methodCallAST(string n,methodArgList *m);
    string str();
    Value* Codegen();
};
class block{
    typedSymbolList *var_decl_list;
    StatementList *statement_list;
public:
    block(typedSymbolList *vdList,StatementList *statement);
    string str();
    Value*Codegen();
};
class methodBlock{
    typedSymbolList *var_decl_list;
    StatementList *statement_list;
public:
    methodBlock(typedSymbolList *vdList,StatementList *statement);
    string str();
    Value*Codegen();
};
class decafAST {
public:
  virtual ~decafAST() {}
  virtual string str() { return string(""); }
};


class descriptor {
    int lineNo;
    string type;
    //AllocaInst *alloc_a;
    //GlobalVariable *global;
    Value *val;
    BasicBlock *bb;
    Function *func;
    methodBlock *mblock;
    bool flag=false;
    list<string> args_name;
    bool isArray=false;
public:
    descriptor(Value *v) : val(v){}
    descriptor(int x, string t) : lineNo(x),type(t){}
    descriptor(BasicBlock *b):bb(b){}
    descriptor(Function *f):func(f){}
    descriptor(Function *f,methodBlock *mb,list<string> a):func(f),mblock(mb),args_name(a){}
    //descriptor(GlobalVariable *g) : global(g){}
    int getLineNo() {return lineNo;}
    string getType(){return type;}
    void setLineNo(int val) {lineNo=val;}
    void setType(string t){type=t;}
    void setBlock(methodBlock *mb){mblock=mb;}
    void setFlag(){flag=true;}
    void setisArray(){isArray=true;}
    Value *getAlloca(){return val;}
    BasicBlock *getBB(){return bb;}
    Function *getFunc(){return func;}
    methodBlock *getBlock(){return mblock;}
    list<string> getListArgs() {return args_name;}
    bool getFlag(){return flag;}
    bool getIsArray(){return isArray;}
    //GlobalVariable *getGlobal(){return global;}
};
class symboltable {

public:

  symboltable() {
  }

  void new_symtbl() {
    symbol_table *new_symtbl = new symbol_table();
    symtbl.push_front(new_symtbl);
  }

  void pop_symtbl() {
    if (symtbl.empty())
      throw runtime_error("no symbol table to remove here!");
    symtbl.pop_front();
  }

  void remove_symtbl() {
    symbol_table *tbl;
    if (symtbl.empty())
      throw runtime_error("no symbol table to remove here!");
    else
      tbl = symtbl.front();
    tbl->clear();
    delete(tbl);
    symtbl.pop_front();
  }

  void enter_symtbl(string ident, descriptor *d) {
    symbol_table *tbl;
    symbol_table::iterator find_ident;

    if (symtbl.empty())
      throw runtime_error("no symbol table created yet!");

    tbl = symtbl.front();
    if ((find_ident = tbl->find(ident)) != tbl->end()) {
      cerr << "Warning: redefining previously defined identifier: " << ident << endl;
      //delete(find_ident->second);
      tbl->erase(ident);
    }
    (*tbl)[ident] = d;
  }
    
  descriptor* access_symtbl(string ident) {
    for (symbol_table_list::iterator i = symtbl.begin(); i != symtbl.end(); ++i) {
      symbol_table::iterator find_ident;
      if ((find_ident = (*i)->find(ident)) != (*i)->end()) return find_ident->second;
    }
    return NULL;
  }

private:
  typedef map<string, descriptor* > symbol_table;
  typedef list<symbol_table* > symbol_table_list;
  symbol_table_list symtbl;
  
};

symboltable *symtbl;
class Expression;
string getStringBlock(block *d);
string getStringMBlock(methodBlock *d);
string getStringMList(methodArgList *d);
string getStringMCall(methodCallAST *d);
string getString(decafAST *d) {
	if (d != NULL) {
		return d->str();
	} else {
		return string("None");
	}
}

Type *getLLVMType (string ty){
    if(ty=="IntType")
        return Builder.getInt32Ty();
    else if(ty=="StringType")
        return Builder.getInt8PtrTy();
    else if(ty=="BoolType")
        return Builder.getInt1Ty();
    else
        return Builder.getVoidTy();
}

template <class T>
string commaList(list<T> vec) {
    string s("");
    for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) { 
        s = s + (s.empty() ? string("") : string(",")) + (*i)->str(); 
    }   
    if (s.empty()) {
        s = string("None");
    }   
    return s;
}
template <class T>
Value *listCodegen(list<T> vec) {
    Value *val = NULL;
    for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) {
        Value *j = (*i)->Codegen();
        if (j != NULL) { val = j; }
    }
    return val;
}
template <class T>
Function *listCodegenFunc(list<T> vec) {
    Function *val = NULL;
    for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) {
        Function *j = (*i)->Codegen();
        if (j != NULL) { val = j; }
    }
    return val;
}
template <class T>
Function *listCodegenFuncForMethod(list<T> vec) {
    Function *val = NULL;
    //symtbl->new_symtbl();
    for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) {
          (*i)->Precodegen();
           //if (j != NULL) { val = j; }
       }
    descriptor *checkMain=symtbl->access_symtbl("main");
    if(checkMain==NULL){
        throw runtime_error("main does not exist");
    }
    for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) {
          Function *j = (*i)->Codegen();
          if (j != NULL) { val = j; }
      }
    return val;
}
template <class T>
AllocaInst *listCodegenAlloca(list<T> vec) {
    AllocaInst *val = NULL;
    for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) {
        AllocaInst *j = (*i)->Codegen();
        if (j != NULL) { val = j; }
    }
    return val;
}


class ConstantAST : public decafAST{
    int valInt;
    string valBool;
    char *c;
public:
    ConstantAST(string val): valBool(val){}
    ConstantAST(int val): valInt(val){}
    ConstantAST(char *ch): c(ch){}
    string str(){
        if(valBool != "True" && valBool != "False"){
            if(c==nullptr)
                return string("NumberExpr(")+to_string(valInt)+")";
            else{
                char ch;
                int temp=0;
                for(int i=0;i<strlen(c);i++){
                    if(c[i]=='\\')
                       {
                        temp=c[i+1];
                        if(temp==110)
                           ch='\n';
                        else if(temp==114)
                           ch='\r';
                        else if(temp==116)
                           ch='\t';
                        else if(temp==118)
                           ch='\v';
                        else if(temp==102)
                           ch='\f';
                        else if(temp==97)
                           ch='\a';
                        else if(temp==98)
                           ch='\b';
                        else if(temp==92)
                            ch='\\';
                        else if(temp==39)
                            ch='\'';
                        else if(temp==34)
                            ch='\"';
                        break;
                    }
                }
                if(temp==0)
                    ch=c[1];
                return string("NumberExpr(")+to_string(ch)+")";
            }
        }
        else{
            if(valBool=="True")
                return string ("BoolExpr(True)");
            else
                return string ("BoolExpr(False)");
        }
    }
    Constant *Codegen(){
        if(valBool!="True"&&valBool!="False"){
            if(c==nullptr)
                return Builder.getInt32(valInt);
            else{
                char ch;
                int temp=0;
                for(int i=0;i<strlen(c);i++){
                    if(c[i]=='\\')
                       {
                        temp=c[i+1];
                        if(temp==110)
                           ch='\n';
                        else if(temp==114)
                           ch='\r';
                        else if(temp==116)
                           ch='\t';
                        else if(temp==118)
                           ch='\v';
                        else if(temp==102)
                           ch='\f';
                        else if(temp==97)
                           ch='\a';
                        else if(temp==98)
                           ch='\b';
                        else if(temp==92)
                            ch='\\';
                        else if(temp==39)
                            ch='\'';
                        else if(temp==34)
                            ch='\"';
                        break;
                    }
                }
                if(temp==0)
                    ch=c[1];
                //GlobalVariable *GS=Builder.CreateGlobalString(c,"globalstring");
                //return Builder.CreateConstGEP2_32(GS->getValueType(),GS,0,0,"cast");
                return Builder.getInt32(ch);
            }
        }
        else{
            if(valBool=="True")
                return Builder.getInt1(1);
            else
                return Builder.getInt1(0);
        }
    }
};

class arrayAST : public decafAST{
    int size;
    string type;
public:
    arrayAST(int s, string t): size(s),type(t){}
    string getType(){
        return type;
    }
    int getSize(){
        return size;
    }
    string str(){
        return string("Array(")+to_string(size)+")";
    }
    ArrayType *Codegen(){
        ArrayType *arrayi32 = ArrayType::get(getLLVMType(type), size);
         // zeroinitalizer: initialize array to all zeroes
        return arrayi32;
    }
};

class decafStmtList : public decafAST {
    list<decafAST *> stmts;
public:
    decafStmtList() {}
    ~decafStmtList() {
        for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++) {
            delete *i;
        }
    }
    int size() { return stmts.size(); }
    void push_front(decafAST *e) { stmts.push_front(e); }
    void push_back(decafAST *e) { stmts.push_back(e); }
    string str() { return commaList<class decafAST *>(stmts); }
};
string commaListString(list<string> vec) {
    string s("");
    for (list<string>::iterator i = vec.begin(); i != vec.end(); i++) {
        s = s + (s.empty() ? string("") : string(",")) + (*i);
    }
    if (s.empty()) {
        s = string("None");
    }
    return s;
}

class externTypeList : public decafAST {
    list<string> type;
public:
    externTypeList() {}
    void push_front(string s)  {type.push_front(s);}
    void push_back(string s)  {type.push_back(s);}
    bool isEmpty() {return type.empty();}
    void pop_front() {type.pop_front();}
    string front() {return type.front();}
    string str() { return commaListString(type); }
};

class IDList : public decafStmtList {
    list<string> name;
public:
    IDList() {}
    void push_front(string s)  {name.push_front(s);}
    void push_back(string s)  {name.push_back(s);}
    void pop_front() {name.pop_front();}
    string front() {return name.front();}
    string str() { return commaListString(name); }
    bool isEmpty() { return name.empty();}
};

class typedSymbol : public decafAST{
    string name;
    string type;
public:
    typedSymbol() {}
    typedSymbol(string n,string t):name(n),type(t){}
    string str() {
        return string("VarDef(")+(name)+","+type+")";
    }
    string getType(){
        return type;
    }
    string getName(){
        return name;
    }
    AllocaInst *Codegen(){
        AllocaInst *Alloca=Builder.CreateAlloca(getLLVMType(type),0,name);
        Value *val;
        if(getLLVMType(type)==Builder.getInt32Ty())
            val=Builder.CreateStore(Builder.getInt32(0),Alloca);
        else if(getLLVMType(type)==Builder.getInt1Ty())
            val=Builder.CreateStore(Builder.getInt1(0),Alloca);
        descriptor *d=new descriptor(Alloca);
        symtbl->enter_symtbl(name,d);
        return Alloca;
    }
};

class typedSymbolList : public decafStmtList{
    list<typedSymbol *> type_list;
public:
    typedSymbolList() {}
    void push_front(typedSymbol *s)  {type_list.push_front(s);}
    void push_back(typedSymbol *s)  {type_list.push_back(s);}
    typedSymbol *front(){return type_list.front();}
    bool isEmpty(){return type_list.empty();}
    void pop_front(){type_list.pop_front();}
    string str() { return commaList(type_list); }
    AllocaInst *Codegen(){
        return listCodegenAlloca<typedSymbol *>(type_list);
    }
};

class Expression : public decafAST {
    string c;
    Expression *op1;
    Expression *op2;
    ConstantAST *cons;
    methodCallAST *m;
public:
    Expression(ConstantAST *co) : cons(co){}
    Expression(string name): c(name){}
    Expression(Expression *x,string name): op2(x),c(name){}
    Expression(string unary,Expression *x): c(unary),op1(x){}
    Expression(string binary,Expression *x,Expression *y): c(binary),op1(x),op2(y){}
    Expression(methodCallAST *mcall): m(mcall){}
    string str(){
        if(op2==nullptr&&op1==nullptr){
            if(cons!=nullptr)
                return getString(cons);
            else if(cons==nullptr&&m==nullptr)
                return string("VariableExpr(")+c+")";
            else if(cons==nullptr&&m!=nullptr)
                return getStringMCall(m);
        }
        else if(op2!=nullptr&&op1!=nullptr){
            return string("BinaryExpr(")+c+","+getString(op1)+","+getString(op2)+")";
        }
        else if (op1!=nullptr&&op2==nullptr){
            if(c=="Minus")
                return string("UnaryExpr(UnaryMinus")+","+getString(op1)+")";
            else
                return string("UnaryExpr(")+c+","+getString(op1)+")";
        }
        else if( op1==nullptr&&op2!=nullptr)
            return string("ArrayLocExpr(")+c+","+getString(op2)+")";
    }
    Value *Codegen(){
        Value *val=NULL;
        if(op2==nullptr && op1==nullptr){
            if(cons!=nullptr)
                val=cons->Codegen();
            else if(cons==nullptr&&m==nullptr){
                descriptor *d=symtbl->access_symtbl(c);
                Value *v=d->getAlloca();
                val=Builder.CreateLoad(v,c);
            }
            else if(cons==nullptr&&m!=nullptr)
                val=m->Codegen();
        }
        else if(op2!=nullptr && op1!=nullptr){
            Value *LHS=op1->Codegen();
            Value *RHS;
            if(c=="Plus")
            {
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt32Ty() && RHS->getType()!=Builder.getInt32Ty())
                    throw runtime_error("invalide type: must be interger");
                val=Builder.CreateAdd(LHS,RHS,"addtmp");
            }
            else if (c=="Minus"){
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt32Ty() && RHS->getType()!=Builder.getInt32Ty())
                    throw runtime_error("invalide type: must be interger");
                val=Builder.CreateSub(LHS,RHS,"subtmp");
            }
            else if (c=="Mult"){
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt32Ty() && RHS->getType()!=Builder.getInt32Ty())
                    throw runtime_error("invalide type: must be interger");
                val=Builder.CreateMul(LHS,RHS,"multmp");
            }
            else if (c=="Div"){
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt32Ty() && RHS->getType()!=Builder.getInt32Ty())
                    throw runtime_error("invalide type: must be interger");
                val=Builder.CreateSDiv(LHS,RHS,"sdivtmp");
            }
            else if (c=="Leftshift"){
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt32Ty() && RHS->getType()!=Builder.getInt32Ty())
                    throw runtime_error("invalide type: must be interger");
                val=Builder.CreateShl(LHS,RHS,"shltemp");
            }
            else if (c=="Rightshift"){
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt32Ty() && RHS->getType()!=Builder.getInt32Ty())
                    throw runtime_error("invalide type: must be interger");
                val=Builder.CreateLShr(LHS,RHS,"lshrtemp");
            }
            else if(c=="Mod"){
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt32Ty() && RHS->getType()!=Builder.getInt32Ty())
                    throw runtime_error("invalide type: must be interger");
                val=Builder.CreateSRem(LHS,RHS,"sremtemp");
            }
            else if(c=="Lt"){
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt32Ty() && RHS->getType()!=Builder.getInt32Ty())
                    throw runtime_error("invalide type: must be interger");
                val=Builder.CreateICmpSLT(LHS,RHS,"slttemp");
            }
            else if(c=="Rt"){
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt32Ty() && RHS->getType()!=Builder.getInt32Ty())
                    throw runtime_error("invalide type: must be interger");
                val=Builder.CreateICmpSGT(LHS,RHS,"sgttemp");
            }
            else if(c=="Leq"){
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt32Ty() && RHS->getType()!=Builder.getInt32Ty())
                    throw runtime_error("invalide type: must be interger");
                val=Builder.CreateICmpSLE(LHS,RHS,"sletemp");
            }
            else if(c=="Neq"){
                RHS=op2->Codegen();
                if(LHS->getType()!= RHS->getType())
                    throw runtime_error("invalide type: must be the same");
                val=Builder.CreateICmpNE(LHS,RHS,"netemp");
            }
            else if(c=="Geq"){
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt32Ty() && RHS->getType()!=Builder.getInt32Ty())
                    throw runtime_error("invalide type: must be interger");
                val=Builder.CreateICmpSGE(LHS,RHS,"sgetemp");
            }
            else if(c=="Eq"){
                RHS=op2->Codegen();
                if(LHS->getType()!= RHS->getType())
                    throw runtime_error("invalide type: must be the same");
                val=Builder.CreateICmpEQ(LHS,RHS,"eqtemp");
            }
            else if(c=="And"){
                Function *func = Builder.GetInsertBlock()->getParent();
                BasicBlock *init=Builder.GetInsertBlock();
                BasicBlock *skctEndBB=BasicBlock::Create(TheContext,"skctend",func);
                BasicBlock *noskctBB=BasicBlock::Create(TheContext,"noskct",func);
                descriptor *dskct=new descriptor(skctEndBB);
                descriptor *dnoskct=new descriptor(noskctBB);
                symtbl->enter_symtbl(string("skctend"),dskct);
                symtbl->enter_symtbl(string("noskct"),dnoskct);
                Builder.CreateCondBr(LHS,noskctBB,skctEndBB);
                Builder.SetInsertPoint(noskctBB);
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt1Ty() && RHS->getType()!=Builder.getInt1Ty())
                    throw runtime_error("invalid type: must be boolean");
                val=Builder.CreateAnd(LHS,RHS,"andtmp");
                Builder.CreateBr(skctEndBB);
                Builder.SetInsertPoint(skctEndBB);
                PHINode *phival = Builder.CreatePHI(LHS->getType(), 2, "phival");
                phival->addIncoming(LHS, init);
                phival->addIncoming(val, noskctBB);
                return phival;
            }
                
            else if(c=="Or")
            {
                Function *func = Builder.GetInsertBlock()->getParent();
                BasicBlock *init=Builder.GetInsertBlock();
                BasicBlock *skctEndBB=BasicBlock::Create(TheContext,"skctend",func);
                BasicBlock *noskctBB=BasicBlock::Create(TheContext,"noskct",func);
                descriptor *dskct=new descriptor(skctEndBB);
                descriptor *dnoskct=new descriptor(noskctBB);
                symtbl->enter_symtbl(string("skctend"),dskct);
                symtbl->enter_symtbl(string("noskct"),dnoskct);
                Builder.CreateCondBr(LHS,skctEndBB,noskctBB);
                Builder.SetInsertPoint(noskctBB);
                RHS=op2->Codegen();
                if(LHS->getType()!=Builder.getInt1Ty() && RHS->getType()!=Builder.getInt1Ty())
                    throw runtime_error("invalid type: must be boolean");
                val=Builder.CreateOr(LHS,RHS,"ortemp");
                Builder.CreateBr(skctEndBB);
                Builder.SetInsertPoint(skctEndBB);
                PHINode *phival = Builder.CreatePHI(LHS->getType(), 2, "phival");
                phival->addIncoming(LHS, init);
                phival->addIncoming(val, noskctBB);
                return phival;
            }
            //return val;
        }
        else if(op1!=nullptr&&op2==nullptr)
        {
            Value *LHS=op1->Codegen();
            if(c=="Minus"){
                if(LHS->getType()!=Builder.getInt32Ty() )
                    throw runtime_error("invalide type: must be boolean");
                val=Builder.CreateNeg(LHS,"negtemp");
            }
            else{
                if(LHS->getType()!=Builder.getInt1Ty() )
                    throw runtime_error("invalide type: must be boolean");
                val=Builder.CreateNot(LHS,"nottemp");
            }
        }
        else if( op1==nullptr&&op2!=nullptr){
            descriptor *d=symtbl->access_symtbl(c);
            if(d->getIsArray()==false){
                throw runtime_error("trying to index a scalar variable");
            }
            Value *v=d->getAlloca();
            Value *index=op2->Codegen();
            if(index->getType()==Builder.getInt1Ty())
                throw runtime_error("trying to index a with a boolean");
            Value *ArrayIndex=Builder.CreateGEP(Builder.getInt32Ty(),v,index,"arrayindex");
            val=Builder.CreateLoad(ArrayIndex,"loadtmp");
        }
        return val;
    }
};

class AssignAST : public decafAST{
        string value;
        Expression *expr;
        Expression *exprIndex;
public:
        AssignAST(string s,Expression *e): value(s),expr(e){}
        AssignAST(string s,Expression *e,Expression *e2): value(s),expr(e),exprIndex(e2){}
        string str(){
            if(exprIndex==nullptr)
                return string("AssignVar(")+(value)+","+getString(expr)+")";
            else
                return string("AssignArrayLoc(")+(value)+","+getString(exprIndex)+","+getString(expr)+")";
        }
    Value *Codegen(){
        if(exprIndex==nullptr){
        descriptor *d=symtbl->access_symtbl(value);
        Value *Alloca=d->getAlloca();
        Value *rvalue=expr->Codegen();
        PointerType *ptrTy=rvalue->getType()->getPointerTo();
        Value *val=NULL;
        if(ptrTy==Alloca->getType())
            val=Builder.CreateStore(rvalue,Alloca);
        return val;
        }
        else{
            descriptor *d=symtbl->access_symtbl(value);
            if(d->getIsArray()==false){
                throw runtime_error("trying to index a scalar variable");
            }
            Value *v=d->getAlloca();
            Value *index=exprIndex->Codegen();
            if(index->getType()==Builder.getInt1Ty())
                throw runtime_error("trying to index a with a boolean");
            Value *ArrayIndex=Builder.CreateGEP(Builder.getInt32Ty(),v,index,"arrayindex");
            Value *rvalue=expr->Codegen();
            Value *ArrayStore=Builder.CreateStore(rvalue,ArrayIndex);
            return ArrayStore;
        }
    }
};

class AssignList : public decafStmtList{
    list<AssignAST *> a_list;
       public:
           AssignList() {}
           void push_front(AssignAST *e) {a_list.push_front(e);}
           void push_back(AssignAST *e) {a_list.push_back(e);}
           string str(){return commaList<class AssignAST *>(a_list);}
    Value *Codegen(){
        return listCodegen<AssignAST*>(a_list);
    }
};
class methodArgAST {
    string value;
    Expression *expr;
public:
    methodArgAST() {}
    methodArgAST(string s): value(s){}
    methodArgAST(Expression *e):expr(e){}
    string str(){
        if(expr==nullptr)
            return string("StringConstant(")+value+")";
        else
            return getString(expr);
    }
    Value *Codegen(){
        if(expr==nullptr)
        {
            //const char*temp=value.c_str();
            char *s=new char(value.length()-2);//strlen(temp));
            int j=0;
            for(int i=1;i<value.length()-1;i++)//strlen(temp);i++)
            {
                //if(i!=0 && i!= strlen(temp)-1){
                    if(value[i]=='\\')
                    {
                            char temp_char;
                            temp_char=value[i+1];
                            if(temp_char=='n'){//110)
                                s[j]='\n';}
                            else if(temp_char=='r'){//114)
                                s[j]='\r';}
                            else if(temp_char=='t'){//116)
                                s[j]='\t';}
                            else if(temp_char=='v'){//118)
                                s[j]='\v';}
                            else if(temp_char=='f'){//102)
                                s[j]='\f';}
                            else if(temp_char=='a'){//97)
                                s[j]='\a';}
                            else if(temp_char=='b'){//98)
                                s[j]='\b';}
                            else if(temp_char=='\\'){//92)
                                s[j]='\\';}
                            else if(temp_char=='\''){//39)
                                s[j]='\'';}
                            else if(temp_char=='\"'){//34)
                                s[j]='\"';}
                        j++;
                        i++;
                    }
                    else{
                        s[j]=value[i];
                        j++;
                    }
                //}
            }
            GlobalVariable *GS=Builder.CreateGlobalString(s,"globalstring");
            return Builder.CreateConstGEP2_32(GS->getValueType(),GS,0,0,"cast");
        }
        else{
            Value* val=expr->Codegen();
            // gives you a pointer to the function definition

            /*if(val->getType()==Builder.getInt1Ty()) //&& func->getReturnType()==Builder.getInt32Ty())
            {
                Value *promo = Builder.CreateZExt(val, Builder.getInt32Ty(), "zexttmp");
                return promo;
            }*/
            //else
            return val;
        }
    }
};

class methodArgList {
    list<methodArgAST *> m_list;
    public:
        methodArgList() {}
        void push_front(methodArgAST *e) {m_list.push_front(e);}
        void push_back(methodArgAST *e) {m_list.push_back(e);}
    void pop_front(){m_list.pop_front();}
    bool isEmpty(){return m_list.empty();}
    methodArgAST* front(){return m_list.front();}
        string str(){return commaList<class methodArgAST *>(m_list);}
    Value *Codegen(){
        return listCodegen<methodArgAST*>(m_list);
    }
};

methodCallAST::methodCallAST(string n,methodArgList *m){
    name=n;
    mlist=m;
}
string methodCallAST::str(){
    return string("MethodCall(")+name+","+getStringMList(mlist)+")";
}
Value* methodCallAST::Codegen(){
    descriptor *DforFunc=symtbl->access_symtbl(name);
    //Value *val;
    Function *call;
    if (DforFunc == NULL) {
        call=TheModule->getFunction(name);
        Value *val;
        if(call==0)
            throw runtime_error("could not find the function\n");
        vector<Value *> args;
        methodArgList *margs=mlist;
        for(auto &Arg:call->args()){
            methodArgAST *temp=margs->front();
            Value *tempVal=temp->Codegen();
            if(Arg.getType()==Builder.getInt32Ty() && tempVal->getType()==Builder.getInt1Ty()){
                Value *promo= Builder.CreateZExt(tempVal, Builder.getInt32Ty(), "zexttmp");
                args.push_back(promo);
            }
            else{
                args.push_back(tempVal);}
            margs->pop_front();
        }
        // argvals are the values in the method call,
        // e.g. foo(1) would have a vector of size one with value of 1 with type i32.
        bool isVoid = call->getReturnType()->isVoidTy();
        val = Builder.CreateCall(
            call,
            args,
            isVoid ? "" : "calltmp"
        );
        return val;
    }
    else{
      call=DforFunc->getFunc();
        Value *val;
        if(DforFunc->getFlag()==false){
            BasicBlock *currentBB=Builder.GetInsertBlock();
            methodBlock *temp=DforFunc->getBlock();
            BasicBlock *BB = BasicBlock::Create(TheContext, "entry", call);
                // insert "entry" into symbol table (not used in HW3 but useful in HW4)
            descriptor *d=new descriptor(BB);
            symtbl->enter_symtbl(string("entry"),d);
                // All subsequent calls to IRBuilder will place instructions in this location
            Builder.SetInsertPoint(BB);
            list<string> args_name=DforFunc->getListArgs();
            for (auto &Arg : call->args()) {
                //typedSymbol *t=tlist2->front();
                //tlist2->pop_front();
                AllocaInst *Alloca = Builder.CreateAlloca(Arg.getType(),0,Arg.getName());
              // Store the initial value into the alloca.
                Builder.CreateStore(&Arg, Alloca);
              // Add to symbol table
                descriptor *d=new descriptor(Alloca);
                symtbl->enter_symtbl(args_name.front(),d);
                args_name.pop_front();
            }
            val=temp->Codegen();
            BasicBlock *CurBB=Builder.GetInsertBlock();
            if(CurBB->getTerminator()==NULL){
                string boolty="BoolType";
                string intty="IntType";
                if(call->getReturnType()==getLLVMType(intty))
                    Builder.CreateRet(ConstantInt::get(TheContext, APInt(32, 0)));
                else if(call->getReturnType()==getLLVMType(boolty))
                    Builder.CreateRet(ConstantInt::get(TheContext, APInt(1, 1)));
                else{
                    Value *retVal=nullptr;
                    Builder.CreateRet(retVal);
                }
            }
            DforFunc->setFlag();
            Builder.SetInsertPoint(currentBB);
        }
        vector<Value *> args;
        methodArgList *margs=mlist;
        for(auto &Arg:call->args()){
            methodArgAST *temp=margs->front();
            Value *tempVal=temp->Codegen();
            if(Arg.getType()==Builder.getInt32Ty() && tempVal->getType()==Builder.getInt1Ty()){
                Value *promo= Builder.CreateZExt(tempVal, Builder.getInt32Ty(), "zexttmp");
                args.push_back(promo);
            }
            else{
                args.push_back(tempVal);}
            margs->pop_front();
        }
        // argvals are the values in the method call,
        // e.g. foo(1) would have a vector of size one with value of 1 with type i32.
        bool isVoid = call->getReturnType()->isVoidTy();
        val = Builder.CreateCall(
            call,
            args,
            isVoid ? "" : "calltmp"
        );
        return val;
    }
    
}

string getStringMList(methodArgList *d){
    if (d != NULL) {
        return d->str();
    } else {
        return string("None");
    }
}
string getStringMCall(methodCallAST *d){
    if (d != NULL) {
           return d->str();
       } else {
           return string("None");
       }
}

class Statement : public decafAST {
    string value;
    Expression *expr;
    AssignAST *a;
    methodCallAST *m;
    AssignList *prelist;
    AssignList *looplist;
    block *blwhile;
    block *blif;
    block *blelse;
    block *blfor;
    block *b1;
public:
    Statement(string s): value(s){}
    Statement(string s, Expression *e): value(s), expr(e){}
    Statement(AssignAST *x):a(x){}
    Statement(methodCallAST *x): m(x){}
    Statement(Expression *e,block *b):expr(e),blwhile(b){}
    Statement(block *b,Expression *e):expr(e),blif(b){}
    Statement(block *b,block *b2,Expression *e):expr(e),blif(b),blelse(b2){}
    Statement(Expression *e,block *b, AssignList *a1,AssignList*a2)
    :expr(e),blfor(b),prelist(a1),looplist(a2){}
    Statement(block *b): b1(b){}
    string str(){
        if(value=="break")
            return string("BreakStmt");
        else if(value=="continue")
            return string("ContinueStmt");
        else if(value=="return"&&expr==nullptr)
            return string("ReturnStmt(None)");
        else if(value=="return"&&expr!=nullptr)
            return string("ReturnStmt(")+getString(expr)+")";
        else if(a!=nullptr)
            return getString(a);
        else if(m!=nullptr)
            return getStringMCall(m);
        else if(blwhile!=nullptr)
            return string("WhileStmt(")+getString(expr)+","+getStringBlock(blwhile)+")";
        else if(blif!=nullptr)
        {
            if(blelse==nullptr)
                return string("IfStmt(")+getString(expr)+","+getStringBlock(blif)+","+string("None")+")";
            else
                return string("IfStmt(")+getString(expr)+","+getStringBlock(blif)+","+getStringBlock(blelse)+")";
        }
        else if(blfor!=nullptr)
            return string("ForStmt(")+getString(prelist)+","+getString(expr)+","+getString(looplist)+","+getStringBlock(blfor)+")";
        else if(b1!=nullptr)
            return getStringBlock(b1);
    }
    Value *Codegen(){
        Value *val=NULL;
        if(a!=nullptr)
            val=a->Codegen();
        else if(m!=nullptr)
            val=m->Codegen();
        else if(value=="break"){
            descriptor *d=symtbl->access_symtbl(string("end"));
            if(d==NULL){
                d=symtbl->access_symtbl(string("false"));
            }
            BasicBlock *EndBB=d->getBB();
            Builder.CreateBr(EndBB);
            //symtbl->remove_symtbl();
        }
        else if(value=="continue"){
            descriptor *d=symtbl->access_symtbl(string("next"));
            if(d==NULL){
                d=symtbl->access_symtbl(string("loop"));
            }
             if(d==NULL){
                d=symtbl->access_symtbl(string("false"));
            }
            BasicBlock *ContBB=d->getBB();
            Builder.CreateBr(ContBB);
        }
        else if(value=="return" &&expr!=nullptr){
            Function *func=Builder.GetInsertBlock()->getParent();
            if(func->getReturnType()==Builder.getInt32Ty() || func->getReturnType()==Builder.getInt1Ty()){
                Value *temp=expr->Codegen();
                val=Builder.CreateRet(temp);
                if(temp->getType()!=func->getReturnType())
                    throw runtime_error("type mismatch, return value did not match with function");
            }
            else
                throw runtime_error("trying to return a value on void function");
        }
        else if(b1!=nullptr){
            //
            symtbl->new_symtbl();
            val=b1->Codegen();
            symtbl->remove_symtbl();
        }
        else if(blif!=nullptr){
            if(blelse==nullptr){
                Function *func = Builder.GetInsertBlock()->getParent();
                BasicBlock *IfStartBB=BasicBlock::Create(TheContext,"if",func);
                BasicBlock *IfTrueBB=BasicBlock::Create(TheContext,"true",func);
                BasicBlock *EndBB=BasicBlock::Create(TheContext,"false",func);
                descriptor *dIf=new descriptor(IfStartBB);
                descriptor *dtrue=new descriptor(IfTrueBB);
                descriptor *dend=new descriptor(EndBB);
                //
                symtbl->new_symtbl();
                symtbl->enter_symtbl(string("if"),dIf);
                symtbl->enter_symtbl(string("true"),dtrue);
                symtbl->enter_symtbl(string("false"),dend);
                Builder.CreateBr(IfStartBB);
                Builder.SetInsertPoint(IfStartBB);
                Value *checkExpr=expr->Codegen();
                if(checkExpr->getType()!=Builder.getInt1Ty())
                    throw runtime_error("check statement is not boolean");
                Builder.CreateCondBr(checkExpr, IfTrueBB, EndBB);
                Builder.SetInsertPoint(IfTrueBB);
                //symtbl->new_symtbl();
                val=blif->Codegen();
                Builder.CreateBr(EndBB);
                //symtbl->remove_symtbl();
                symtbl->remove_symtbl();
                // pop the symbol table after IfStmt Codegen is done
                Builder.SetInsertPoint(EndBB);
            }
            else{
                Function *func = Builder.GetInsertBlock()->getParent();
                BasicBlock *IfStartBB=BasicBlock::Create(TheContext,"if",func);
                BasicBlock *IfTrueBB=BasicBlock::Create(TheContext,"true",func);
                BasicBlock *ElseBB=BasicBlock::Create(TheContext,"else",func);
                BasicBlock *EndBB=BasicBlock::Create(TheContext,"false",func);
                descriptor *dIf=new descriptor(IfStartBB);
                descriptor *dtrue=new descriptor(IfTrueBB);
                descriptor *delse=new descriptor(ElseBB);
                descriptor *dend=new descriptor(EndBB);
                
                symtbl->new_symtbl();
                symtbl->enter_symtbl(string("if"),dIf);
                symtbl->enter_symtbl(string("true"),dtrue);
                symtbl->enter_symtbl(string("else"),delse);
                symtbl->enter_symtbl(string("false"),dend);
                Builder.CreateBr(IfStartBB);
                Builder.SetInsertPoint(IfStartBB);
                Value *checkExpr=expr->Codegen();
                if(checkExpr->getType()!=Builder.getInt1Ty())
                    throw runtime_error("check statement is not boolean");
                Builder.CreateCondBr(checkExpr, IfTrueBB, ElseBB);
                Builder.SetInsertPoint(IfTrueBB);
                //symtbl->new_symtbl();
                val=blif->Codegen();
                Builder.CreateBr(EndBB);
                //symtbl->remove_symtbl();
                Builder.SetInsertPoint(ElseBB);
                val=blelse->Codegen();
                Builder.CreateBr(EndBB);
                symtbl->remove_symtbl();
                Builder.SetInsertPoint(EndBB);
            }
        }
        else if(blwhile!=nullptr){
            Function *func = Builder.GetInsertBlock()->getParent();
            BasicBlock *LoopBB=BasicBlock::Create(TheContext,"loop",func);
            BasicBlock *BodyBB=BasicBlock::Create(TheContext,"body",func);
            BasicBlock *EndBB=BasicBlock::Create(TheContext,"end",func);
            
            descriptor *dloop=new descriptor(LoopBB);
            descriptor *dbody=new descriptor(BodyBB);
            descriptor *dend=new descriptor(EndBB);
            symtbl->new_symtbl();
            symtbl->enter_symtbl(string("loop"),dloop);
            symtbl->enter_symtbl(string("body"),dbody);
            symtbl->enter_symtbl(string("end"),dend);
            
            Builder.CreateBr(LoopBB);
            Builder.SetInsertPoint(LoopBB);
            Value *checkExpr=expr->Codegen();
            if(checkExpr->getType()!=Builder.getInt1Ty())
                throw runtime_error("check statement is not boolean");
            Builder.CreateCondBr(checkExpr, BodyBB, EndBB);
            Builder.SetInsertPoint(BodyBB);
            val=blwhile->Codegen();
            Builder.CreateBr(LoopBB);
            //symtbl->remove_symtbl();
            
            symtbl->remove_symtbl();
            Builder.SetInsertPoint(EndBB);
            
        }
        else if(blfor!=nullptr){
            Function *func = Builder.GetInsertBlock()->getParent();
            BasicBlock *LoopBB=BasicBlock::Create(TheContext,"loop",func);
            BasicBlock *BodyBB=BasicBlock::Create(TheContext,"body",func);
            BasicBlock *NextBB=BasicBlock::Create(TheContext,"next",func);
            BasicBlock *EndBB=BasicBlock::Create(TheContext,"end",func);
            
            descriptor *dloop=new descriptor(LoopBB);
            descriptor *dbody=new descriptor(BodyBB);
            descriptor *dnext=new descriptor(NextBB);
            descriptor *dend=new descriptor(EndBB);
            
            symtbl->new_symtbl();
            symtbl->enter_symtbl(string("loop"),dloop);
            symtbl->enter_symtbl(string("body"),dbody);
            symtbl->enter_symtbl(string("end"),dend);
            symtbl->enter_symtbl(string("next"),dnext);
            
            val=prelist->Codegen();
            Builder.CreateBr(LoopBB);
            Builder.SetInsertPoint(LoopBB);
            Value *checkExpr=expr->Codegen();
            if(checkExpr->getType()!=Builder.getInt1Ty())
                throw runtime_error("check statement is not boolean");
            Builder.CreateCondBr(checkExpr, BodyBB, EndBB);
            Builder.SetInsertPoint(BodyBB);
            val=blfor->Codegen();
            Builder.CreateBr(NextBB);
            //symtbl->remove_symtbl();
            Builder.SetInsertPoint(NextBB);
            val=looplist->Codegen();
            Builder.CreateBr(LoopBB);
            symtbl->remove_symtbl();
            Builder.SetInsertPoint(EndBB);
        }
        /*else{
        }*/
        return val;
    }
};
class StatementList : public decafStmtList{
     list<Statement *> s_list;
    public:
        StatementList() {}
        void push_front(Statement *e) {s_list.push_front(e);}
        void push_back(Statement *e) {s_list.push_back(e);}
        string str(){return commaList<class Statement *>(s_list);}
    Value *Codegen(){
        return listCodegen<Statement*>(s_list);
    }
};

/*class methodBlock : public decafAST {
    typedSymbolList *var_decl_list;
    StatementList *statement_list;
public:
    methodBlock(typedSymbolList *vdList,StatementList *statement)
    : var_decl_list(vdList),statement_list(statement) {
        symtbl->new_symtbl();
    }
    string str(){
        return string("MethodBlock")+"("+getString(var_decl_list)+","+getString(statement_list)+")";
    }
    Value *Codegen(){
        //symtbl->new_symtbl();
        Value *val=NULL;
        if(NULL!=var_decl_list)
            val=var_decl_list->Codegen();
        if(NULL!=statement_list)
            val=statement_list->Codegen();
        symtbl->remove_symtbl();
        return val;
    }
};*/
methodBlock::methodBlock(typedSymbolList *vdList,StatementList *statement){
    var_decl_list=vdList;
    statement_list=statement;
    //symtbl->new_symtbl();
}
string methodBlock::str(){
    return string("MethodBlock")+"("+getString(var_decl_list)+","+getString(statement_list)+")";
}
Value* methodBlock::Codegen(){
    //symtbl->new_symtbl();
    Value *val=NULL;
    if(NULL!=var_decl_list)
        val=var_decl_list->Codegen();
    if(NULL!=statement_list)
        val=statement_list->Codegen();
    //symtbl->remove_symtbl();
    return val;
}

block::block(typedSymbolList *vdList,StatementList *statement){
    var_decl_list=vdList;
    statement_list=statement;
    //symtbl->new_symtbl();
}
string block::str(){
    return string("Block")+"("+getString(var_decl_list)+","+getString(statement_list)+")";
}
Value* block::Codegen(){
    //symtbl->new_symtbl();
    Value *val=NULL;
    if(NULL!=var_decl_list)
        val=var_decl_list->Codegen();
    if(NULL!=statement_list)
        val=statement_list->Codegen();
    //symtbl->remove_symtbl();
    return val;
}
string getStringBlock(block *d){
    if (d != NULL) {
        return d->str();
    } else {
        return string("None");
    }
}
string getStringMBlock(methodBlock *d){
    if (d != NULL) {
        return d->str();
    } else {
        return string("None");
    }
}

class fieldDeclAST : public decafAST {
    string Name_list;
    string de_Type;
    ConstantAST *value;
    arrayAST *a;
public:
    fieldDeclAST(string name,string type)
      : Name_list(name), de_Type(type){}
    fieldDeclAST(string name,string type,ConstantAST *val)
        : Name_list(name), de_Type(type),value(val){}
    fieldDeclAST(string name,string type,arrayAST *x)
    : Name_list(name), de_Type(type),a(x){}
    string str(){
            if(value==nullptr)
            {
            if(a==nullptr)
                    return string("FieldDecl(")+Name_list+","+de_Type+","+string("Scalar)");
                else
                    return string("FieldDecl(")+Name_list+","+de_Type+","+getString(a)+")";
                
            }
            else
            {    return string("AssignGlobalVar(")+Name_list+","+de_Type+","+getString(value)+")";}
    }
    Value *Codegen(){
        if(value==nullptr){
            if(a==nullptr){
                GlobalVariable *gv;
                if(getLLVMType(de_Type)==Builder.getInt32Ty()){
                  gv = new GlobalVariable(
                  *TheModule,
                  getLLVMType(de_Type),
                  false,  // variable is mutable
                  GlobalValue::ExternalLinkage,
                  Builder.getInt32(0),
                  Name_list);
                }
                else if(getLLVMType(de_Type)==Builder.getInt1Ty()){
                    GlobalVariable *gv = new GlobalVariable(
                                     *TheModule,
                                     getLLVMType(de_Type),
                                     false,  // variable is mutable
                                     GlobalValue::InternalLinkage,
                                     Builder.getInt1(0),
                                     Name_list);
                }
                descriptor *d=new descriptor(gv);
                symtbl->enter_symtbl(Name_list,d);
                return gv;
            }
            else{
                ArrayType *at=a->Codegen();
                Constant *zeroInit = Constant::getNullValue(at);
                GlobalVariable *gv = new GlobalVariable(*TheModule, at, false, GlobalValue::ExternalLinkage, zeroInit, Name_list);
                Value *ArrayLoc = Builder.CreateStructGEP(at, gv, 0, "arrayloc");
                descriptor *d=new descriptor(ArrayLoc);
                d->setisArray();
                symtbl->enter_symtbl(Name_list,d);
                return gv;
            }
        }
        else{
            GlobalVariable *gv = new GlobalVariable(
              *TheModule,
              getLLVMType(de_Type),
              false,  // variable is mutable
              GlobalValue::ExternalLinkage,
              value->Codegen(),
              Name_list);
            descriptor *d=new descriptor(gv);
            symtbl->enter_symtbl(Name_list,d);
            return gv;
        }
    }
};

class fieldDeclList : public decafStmtList {
    list<fieldDeclAST *> field_list;
public:
    fieldDeclList() {}
    void push_front(fieldDeclAST *e) {field_list.push_front(e);}
    void push_back(fieldDeclAST *e) {field_list.push_back(e);}
    string str(){return commaList<class fieldDeclAST *>(field_list);}
    Value *Codegen(){
        return listCodegen<fieldDeclAST *>(field_list);
    }
        
};
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          const string &VarName) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(Type::getDoubleTy(TheContext), nullptr, VarName);
}

class methodDeclAST : public decafAST {
    string func_Name;
    string method_type;
    typedSymbolList *param_list;
    methodBlock *block;
    list<string> args_name;
public:
    methodDeclAST(string name,string mType, typedSymbolList *pList, methodBlock  *b)
    : func_Name(name),method_type(mType),param_list(pList),block(b){
    }
    string str(){
        return string("Method")+"("+func_Name+","+method_type+","+getString(param_list)+","+getStringMBlock(block)+")";
    }
    void Precodegen(){
        Type *returnTy=getLLVMType(method_type);
        vector<Type*>args;
        //list<string> args_name;
        typedSymbolList *tlist=param_list;
        while(!tlist->isEmpty()){
            typedSymbol *t=tlist->front();
            tlist->pop_front();
            Type* ty=getLLVMType(t->getType());
            args_name.push_back(t->getName());
            args.push_back(ty);
        }
        Function *func=Function::Create(FunctionType::get(returnTy,args,false),Function::ExternalLinkage,func_Name,TheModule);
        descriptor *d=new descriptor(func,block,args_name);
        symtbl->enter_symtbl(func_Name,d);
    }
    Function *Codegen(){
        descriptor *DforFunc=symtbl->access_symtbl(func_Name);
        Function *func=DforFunc->getFunc();
        if(DforFunc->getFlag()==false){
        DforFunc->setFlag();//set to true
        //symtbl->enter_symtbl(func_Name,DforFunc);
        BasicBlock *BB = BasicBlock::Create(TheContext, "entry", func);
            // insert "entry" into symbol table (not used in HW3 but useful in HW4)
        descriptor *d=new descriptor(BB);
        symtbl->enter_symtbl(string("entry"),d);
            // All subsequent calls to IRBuilder will place instructions in this location
        Builder.SetInsertPoint(BB);
        for (auto &Arg : func->args()) {
            AllocaInst *Alloca = Builder.CreateAlloca(Arg.getType(),0,Arg.getName());
          // Store the initial value into the alloca.
            Builder.CreateStore(&Arg, Alloca);
            descriptor *d=new descriptor(Alloca);
            symtbl->enter_symtbl(args_name.front(),d);
            args_name.pop_front();
          // Add to symbol table
        }
        Value *val=block->Codegen();
        BasicBlock *CurBB=Builder.GetInsertBlock();
        if(CurBB->getTerminator()==NULL){
            string boolty="BoolType";
            string intty="IntType";
            if(func->getReturnType()==getLLVMType(intty))
                Builder.CreateRet(ConstantInt::get(TheContext, APInt(32, 0)));
            else if(func->getReturnType()==getLLVMType(boolty))
                Builder.CreateRet(ConstantInt::get(TheContext, APInt(1, 1)));
            else{
                Value *retVal=nullptr;
                Builder.CreateRet(retVal);
            }
        }
        }
        return func;
    }
    
};

class methodDeclList : public decafStmtList {
        list<methodDeclAST *> method_list;
    public:
        methodDeclList() {}
        void push_front(methodDeclAST *e) {method_list.push_front(e);}
        void push_back(methodDeclAST *e) {method_list.push_back(e);}
        string str() { return commaList<class methodDeclAST *>(method_list); }
        Function *Codegen(){
            //symtbl->new_symtbl();
            //Function *temp=listCodegenFuncForMethod<methodDeclAST *>(method_list);
            return listCodegenFuncForMethod<methodDeclAST *>(method_list);
        }
};

class PackageAST : public decafAST {
	string Name;
	fieldDeclList *f;
	methodDeclList *m;
public:
	PackageAST(string name, fieldDeclList *fieldlist, methodDeclList *methodlist)
		: Name(name), f(fieldlist), m(methodlist) {}
	~PackageAST() { 
		if (f != NULL) { delete f; }
		if (m != NULL) { delete m; }
	}
	string str() { 
		return string("Package") + "(" + Name + "," + getString(f) + "," + getString(m) + ")";
	}
    Value *Codegen() {
        llvm::Value *val = NULL;
        TheModule->setModuleIdentifier(llvm::StringRef(Name));
        symtbl->new_symtbl();
        if (NULL != f) {
            val = f->Codegen();
        }
        if (NULL != m) {
            val = m->Codegen();
        }
        // Q: should we enter the class name into the symbol table?
        return val;
    }
};

class externFuncAST : public decafAST {
    string Name;
    string method_type;
    externTypeList *extern_type_list;
public:
    externFuncAST(string name, string methodType, externTypeList *externType)
    : Name(name), method_type(methodType), extern_type_list(externType){}
    externFuncAST(string name, string methodType)
    : Name(name), method_type(methodType),extern_type_list(new externTypeList()){}
    string str(){
        if(extern_type_list==nullptr)
            return string("ExternFunction") + "("+Name+","+method_type+","+string("None)");
        else
            return string("ExternFunction") + "("+Name+","+method_type+","+string("VarDef(")+getString(extern_type_list)+")"+")";
    }
    Function *Codegen(){
        Type *returnTy=getLLVMType(method_type);
        vector<Type*>args;
        externTypeList *ext_list=extern_type_list;
        //symtbl->new_symtbl();
        while(!ext_list->isEmpty()){
            Type* ty=getLLVMType(ext_list->front());
            ext_list->pop_front();
            args.push_back(ty);
        }
        Function *func=Function::Create(FunctionType::get(returnTy,args,false),Function::ExternalLinkage,Name,TheModule);
        //descriptor *d=new descriptor(func);
        //d->setFlag();
        //symtbl->enter_symtbl(Name,d);
        return func;
    }
};

class externFuncList : public decafStmtList {
    list<externFuncAST *> ext_list;
public:
    externFuncList() {}
    void push_front(externFuncAST *e) {ext_list.push_front(e);}
    void push_back(externFuncAST *e) {ext_list.push_back(e);}
    string str() { return commaList<class externFuncAST *>(ext_list); }
    Function *Codegen() {
        return listCodegenFunc<externFuncAST *>(ext_list);
    }
};

/// ProgramAST - the decaf program
class ProgramAST : public decafAST {
	externFuncList *ExternList;
	PackageAST *PackageDef;
public:
	ProgramAST(externFuncList *externs, PackageAST *c) : ExternList(externs), PackageDef(c) {}
	~ProgramAST() { 
		if (ExternList != NULL) { delete ExternList; } 
		if (PackageDef != NULL) { delete PackageDef; }
	}
	string str() { return string("Program") + "(" + getString(ExternList) + "," + getString(PackageDef) + ")"; }
    Value *Codegen() {
        Value *val = NULL;
        if (NULL != ExternList) {
            val = ExternList->Codegen();
        }
        if (NULL != PackageDef) {
            val = PackageDef->Codegen();
        } else {
            throw runtime_error("no package definition in decaf program");
        }
        return val;
    }
};



