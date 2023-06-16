%{
#include <iostream>
#include <ostream>
#include <string>
#include <cstdlib>
#include <map>
#include <list>
#include "default-defs.h"

int yylex(void);
int yyerror(char *);

// print AST?
bool printAST = false;

#include "decafcomp.cc"

using namespace std;
using namespace llvm;


%}
%define "parse.error" "verbose"




%union{
    class desciptor *desc;
    class symbol_table *symbolt;
    class decafAST *ast;
    class methodDeclAST *mast;
    class methodDeclList *mlist;
    class fieldDeclAST *fast;
    class fieldDeclList *flist;
    class decafStmtList *list;
    class externFuncList *elist;
    class externFuncAST *efast;
    class typedSymbol *tsast;
    class methodBlock *mbast;
    class typedSymbolList *tslist;
    class ConstantAST *cast;
    class Expression *east;
    class Statement *sast;
    class StatementList *slist;
    class methodArgAST *maast;
    class methodArgList *malist;
    class methodCallAST *mcast;
    class AssignAST *aast;
    class AssignList *alist;
    class arrayAST *arast;
    class block *bast;
    class IDList *idlist;
    class externTypeList *etlist;
    string *sval;
    int ival;
    char *cval;
    bool bval;
}

%token T_PACKAGE T_FUNC T_RPAREN  T_LPAREN T_SEMICOLON T_EXTERN T_WHILE T_FOR T_IF T_ELSE
%token T_LCB T_VAR T_COMMA T_LSB T_RSB
%token T_RCB  T_ASSIGN
%token <sval> T_ID T_BOOLTYPE T_INTTYPE T_STRINGTYPE T_VOID  T_MINUS T_BREAK T_CONTINUE T_RETURN
%token <sval> T_NOT T_PLUS T_MULT T_DIV T_LEFTSHIFT T_RIGHTSHIFT T_LT T_GT T_MOD T_LEQ T_GEQ T_EQ T_NEQ T_AND T_OR T_STRINGCONSTANT
%token <sval> T_FALSE T_TRUE
%token <ival> T_INTCONSTANT
%token <cval> T_CHARCONSTANT
%type <ast>  decafpackage
%type <mast> methodDecl
%type <fast> fieldDecl
%type <elist> extern_list
%type <mlist>methodDecls
%type <flist>fieldDecls
%type <tsast> typedSymbol idtype
%type <tslist> typedSymbols idtypes
%type <mbast> mblock
%type <bast> block
%type <cast> constant
%type <mcast> methodcall
%type <maast> methodarg
%type <malist> methodargs
%type <sast> statement
%type <east> expr
%type <aast> assign
%type <alist> assigns
%type <slist> statements
%type <arast> arrayType
%type <sval> methodType type externType boolConstant
%type <etlist> externTypes
%type <idlist> ids
%type <efast> externFunc

%left T_OR
%left T_AND
%left T_EQ T_NEQ T_GT T_LT T_LEQ T_GEQ
%left T_PLUS T_MINUS
%left T_MULT T_DIV T_MOD T_LEFTSHIFT T_RIGHTSHIFT
%left T_NOT
%right UMINUS
%%

start: program

program: extern_list decafpackage
    { 
        ProgramAST *prog = new ProgramAST((externFuncList *)$1, (PackageAST *)$2);
		if (printAST) {
			cout << getString(prog) << endl;
		}
        try {
            Value *val=prog->Codegen();
        }
        catch (std::runtime_error &e) {
            cout << "semantic error: " << e.what() << endl;
            //cout << prog->str() << endl;
            exit(EXIT_FAILURE);
        }
        delete prog;
    }
    | decafpackage
    {
        ProgramAST *prog = new ProgramAST(new externFuncList(), (PackageAST *)$1);
        if (printAST) {
            cout << getString(prog) << endl;
        }
        delete prog;
    }


extern_list: externFunc
{
    externFuncList *list=new externFuncList();
    list->push_back($1);
    $$=list;
}
| extern_list externFunc
{
    externFuncList *list=$1;
    list->push_back($2);
    $$=list;
}
externFunc:
    T_EXTERN T_FUNC T_ID T_LPAREN externTypes T_RPAREN methodType T_SEMICOLON
    {
        externFuncAST *ext= new externFuncAST(*$3, *$7, $5);
        $$=ext;
    }
    | T_EXTERN T_FUNC T_ID T_LPAREN T_RPAREN methodType T_SEMICOLON
    {
        externFuncAST *ext= new externFuncAST(*$3, *$6);
        $$=ext;
    }

decafpackage: T_PACKAGE T_ID begin_block fieldDecls methodDecls end_block
    { $$ = new PackageAST(*$2, (fieldDeclList *)$4, (methodDeclList *)$5); delete $2; }
    | T_PACKAGE T_ID T_LCB T_RCB
{
    $$ = new PackageAST(*$2, new fieldDeclList(), new methodDeclList()); delete $2;
}
    | T_PACKAGE T_ID begin_block fieldDecls end_block
{ $$ = new PackageAST(*$2, (fieldDeclList *)$4, new methodDeclList()); delete $2; }
    | T_PACKAGE T_ID begin_block methodDecls end_block
{
    $$ = new PackageAST(*$2, new fieldDeclList(), (methodDeclList *)$4);
    /*PackageAST *pack=new PackageAST(*$2, new fieldDeclList(), (methodDeclList *)$4);
    Value *val=pack->Codegen();*/
    delete $2; }
    ;

fieldDecls: fieldDecl
    {
        fieldDeclList *list=new fieldDeclList();
        list->push_back($1);
        $$=list;
    }
| fieldDecls fieldDecl
{
    fieldDeclList *list=$1;
    list->push_back($2);
    $$=list;
}
| T_VAR ids type T_SEMICOLON
{
    fieldDeclList *list=new fieldDeclList();
    IDList *idl=$2;
    while(!idl->isEmpty()){
        fieldDeclAST *t=new fieldDeclAST(idl->front(),*$3);
        idl->pop_front();
        list->push_back(t);
    }
    $$=list;
}
| fieldDecls T_VAR ids type T_SEMICOLON
{
    fieldDeclList *list=$1;;
    IDList *idl=$3;
    while(!idl->isEmpty()){
        fieldDeclAST *t=new fieldDeclAST(idl->front(),*$4);
        idl->pop_front();
        list->push_back(t);
    }
    $$=list;
}
fieldDecl:
    T_VAR T_ID type T_SEMICOLON
    {
        fieldDeclAST *field= new fieldDeclAST(*$2,*$3);
        $$=field;
    }
    | T_VAR T_ID arrayType T_SEMICOLON
    {
              fieldDeclAST *field= new fieldDeclAST(*$2,$3->getType(),$3);
              $$=field;
    }
    | T_VAR T_ID type T_ASSIGN constant T_SEMICOLON
    {
        fieldDeclAST *field= new fieldDeclAST(*$2,*$3,$5);
        $$=field;
    }


methodDecls: methodDecl
{
    methodDeclList *m=new methodDeclList();
    m->push_back($1);
    /*Function *func=m->Codegen();
    verifyFunction(*func);*/
    $$=m;
}
| methodDecls methodDecl
{
    methodDeclList *m=$1;
    m->push_back($2);
    $$=m;
}
methodDecl:
    T_FUNC T_ID T_LPAREN T_RPAREN methodType mblock
    {
        methodDeclAST *m=new methodDeclAST(*$2,*$5,new typedSymbolList(),$6);
        $$=m;
    }
    | T_FUNC T_ID T_LPAREN idtypes T_RPAREN methodType mblock
    {
        methodDeclAST *m=new methodDeclAST(*$2,*$6,$4,$7);
        $$=m;
    }

typedSymbols: typedSymbol
    {
        typedSymbolList *list=new typedSymbolList();
        list->push_back($1);
        $$=list;
    }
    | typedSymbols typedSymbol
    {
        typedSymbolList *list=$1;
        list->push_back($2);
        $$=list;
    }
    | T_VAR ids type T_SEMICOLON
    {
        typedSymbolList *list=new typedSymbolList();
        IDList *idl=$2;
        while(!idl->isEmpty()){
            typedSymbol *t=new typedSymbol(idl->front(),*$3);
            idl->pop_front();
            list->push_back(t);
        }
        $$=list;
    }
    | typedSymbols T_VAR ids type T_SEMICOLON
    {
        typedSymbolList *list=$1;
        IDList *idl=$3;
        while(!idl->isEmpty()){
            typedSymbol *t=new typedSymbol(idl->front(),*$4);
            idl->pop_front();
            list->push_back(t);
        }
        $$=list;
        
    }
typedSymbol:
    T_VAR T_ID type T_SEMICOLON
    {
        /*descriptor *d=symtbl->access_symtbl(*$2);
        if(d==nullptr){
            descriptor *desc=new descriptor(lineno, *$3);
            symtbl->enter_symtbl(*$2,desc);
        }*/
        typedSymbol *t=new typedSymbol(*$2,*$3);
        $$=t;
    }


mblock: begin_block end_block
    {
        $$=new methodBlock(new typedSymbolList(),new StatementList());
    }
    | begin_block typedSymbols statements end_block
       {
           methodBlock *mb= new methodBlock($2,$3);
           $$=mb;
       }
    | begin_block typedSymbols  end_block
    {
        methodBlock *mb= new methodBlock($2,new StatementList());
        $$=mb;
    }
    | begin_block statements end_block
    {
        methodBlock *mb= new methodBlock(new typedSymbolList(),$2);
        $$=mb;
    }

block: begin_block end_block
{
    $$=new block(new typedSymbolList(),new StatementList());
}
| begin_block typedSymbols  statements end_block
   {
       block *mb= new block($2,$3);
       $$=mb;
   }
| begin_block typedSymbols  end_block
{
    block *mb= new block($2,new StatementList());
    $$=mb;
}
| begin_block statements end_block
{
    block *mb= new block(new typedSymbolList(),$2);
    $$=mb;
}

begin_block:T_LCB
end_block:T_RCB

statements: statement
    {
        StatementList *slist=new StatementList();
        slist->push_back($1);
        $$=slist;
    }
    | statements statement
    {
        StatementList *slist=$1;
        slist->push_back($2);
        $$=slist;
    }
statement:
    T_RETURN T_SEMICOLON
    {
          Statement *s=new Statement(*$1);
          $$=s;
    }
    | T_RETURN T_LPAREN T_RPAREN T_SEMICOLON
    {
          Statement *s=new Statement(*$1);
          $$=s;
      }
    | T_RETURN T_LPAREN expr T_RPAREN T_SEMICOLON
    {
          Statement *s=new Statement(*$1,$3);
          $$=s;
    }
    | T_BREAK T_SEMICOLON
    {
        Statement *s=new Statement(*$1);
        $$=s;
    }
    | T_CONTINUE T_SEMICOLON
    {
          Statement *s=new Statement(*$1);
          $$=s;
    }
    | assign T_SEMICOLON
    {
        Statement *s=new Statement($1);
        $$=s;
    }
    | methodcall T_SEMICOLON
    {
        Statement *s=new Statement($1);
        $$=s;
    }
    | T_WHILE T_LPAREN expr T_RPAREN block
    {
        Statement *s=new Statement($3,$5);
        $$=s;
    }
    | T_IF T_LPAREN expr T_RPAREN block T_ELSE block
    {
        Statement *s=new Statement($5,$7,$3);
        $$=s;
    }
    | T_IF T_LPAREN expr T_RPAREN block
    {
        Statement *s=new Statement($5,$3);
        $$=s;
    }
    | T_FOR T_LPAREN assigns T_SEMICOLON expr T_SEMICOLON assigns T_RPAREN block
    {
        Statement *s=new Statement($5,$9,$3,$7);
        $$=s;
    }
    | block
    {
        Statement *s=new Statement($1);
        $$=s;
    }



assigns: assign
{
    AssignList *alist=new AssignList();
    alist->push_back($1);
    $$=alist;
}
| assigns assign
{
    AssignList *alist=$1;
    alist->push_back($2);
    $$=alist;
}
assign: T_ID T_ASSIGN expr
{
    AssignAST *a=new AssignAST(*$1,$3);
    $$=a;
}
| T_ID T_LSB expr T_RSB T_ASSIGN expr
{
    AssignAST *a=new AssignAST(*$1,$6,$3);
    $$=a;
}

expr: expr T_PLUS expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_MINUS expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_MULT expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_DIV expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_LEFTSHIFT expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_RIGHTSHIFT expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_MOD expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_EQ expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_GEQ expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_LEQ expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_NEQ expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_LT expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_GT expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_AND expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
|  expr T_OR expr
{
    Expression *ex=new Expression(*$2,$1,$3);
    $$=ex;
}
| T_NOT expr
{
    Expression *ex=new Expression(*$1,$2);
    $$=ex;
}
|T_MINUS expr %prec UMINUS
{
    Expression *ex=new Expression(*$1,$2);
      $$=ex;
}
| constant
{
    Expression *ex=new Expression($1);
    $$=ex;
}
| T_LPAREN expr T_RPAREN {$$=$2;}
| T_ID T_LSB expr T_RSB
{
    Expression *ex=new Expression($3,*$1);
    $$=ex;
}
| methodcall
{
    Expression *ex=new Expression($1);
    $$=ex;
}
| T_ID
{
    Expression *ex=new Expression(*$1);
    $$=ex;
}

methodcall: T_ID T_LPAREN methodargs T_RPAREN
{
    methodCallAST *m=new methodCallAST(*$1,$3);
    $$=m;
}
| T_ID T_LPAREN T_RPAREN
{
    methodCallAST *m=new methodCallAST(*$1,new methodArgList());
    $$=m;
}

methodargs: methodarg
{
    methodArgList *mlist=new methodArgList();
    mlist->push_back($1);
    $$=mlist;
}
| methodargs T_COMMA methodarg
{
    methodArgList *mlist=$1;
    mlist->push_back($3);
    $$=mlist;
}

methodarg:expr
{
    methodArgAST *m=new methodArgAST($1);
    $$=m;
}
| T_STRINGCONSTANT
{
    methodArgAST *m=new methodArgAST(*$1);
    $$=m;
}

arrayType: T_LSB T_INTCONSTANT T_RSB type
{
    arrayAST *a=new arrayAST($2,*$4);
    $$=a;
}

externTypes : externType
{
    externTypeList *ext= new externTypeList();
    ext->push_back(*$1);
    $$=ext;
}
| externTypes T_COMMA externType
{
    externTypeList *ext=$1;
    ext->push_back(*$3);
    $$=ext;
}
externType: T_STRINGTYPE {$$=$1;}
| type


ids : T_ID T_COMMA T_ID
{
    IDList *id=new IDList();
    id->push_back(*$1);
    id->push_back(*$3);
    $$=id;
}
| ids T_COMMA T_ID
{
    IDList *id=$1;
    id->push_back(*$3);
    $$=id;
}


idtypes: idtype
{
    typedSymbolList *list=new typedSymbolList();
    list->push_back($1);
    $$=list;
}
| idtypes T_COMMA idtype
{
    typedSymbolList *list=$1;
    list->push_back($3);
    $$=list;
}
idtype: T_ID type{
    typedSymbol *t=new typedSymbol(*$1,*$2);
    $$=t;
}

methodType: T_VOID {$$=$1;}
| type
type: T_INTTYPE{ $$=$1  ;}
| T_BOOLTYPE {$$=$1;}

constant: T_INTCONSTANT
    {ConstantAST *c=new ConstantAST($1);
        $$=c;}
    | T_CHARCONSTANT
    {
        ConstantAST *c=new ConstantAST($1);
        $$=c;
    }
    | boolConstant
    {
            ConstantAST *c=new ConstantAST(*$1);
            $$=c;
    }

boolConstant: T_TRUE{$$=$1;}
    | T_FALSE{$$=$1;}



%%
int main() {
  symtbl= new symboltable();
  TheModule=new Module("test",TheContext);
  // parse the input and create the abstract syntax tree
  int retval = yyparse();
  TheModule->print(errs(),nullptr);
  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
}

