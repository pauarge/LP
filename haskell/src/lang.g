#header
<<
#include <string>
#include <iostream>
#include <iomanip>
#include <cstdlib>

using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr,int ttype, char *textt);
>>

<<

// Global structures
AST *root;

// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
/*  if (type == ID) {
	attr->kind = "id";
	attr->text = text;
  }
  else {*/
    attr->kind = text;
    attr->text = "";
//  }
}


// function to create a new AST node
AST *createASTnode(Attrib *attr, int type, char *text) {
    AST *as = new AST;
    as->kind = attr->kind;
    as->text = attr->text;
    as->right = NULL;
    as->down = NULL;
    return as;
}


/// create a new "list" AST node with one element
AST *createASTlist(AST *child) {
    AST *as = new AST;
    as->kind = "list";
    as->right = NULL;
    as->down = child;
    return as;
}


/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST *child(AST *a, int n) {
    AST *c = a->down;
    for (int i = 0; c != NULL && i < n; i++) c = c->right;
    return c;
}


// PRINT FUNCTIONS
void ASTPrintIndent(AST *a, string s) {
    if (a == NULL) return;

    cout << a->kind;
    if (a->text != "") cout << "(" << a->text << ")";
    cout << endl;

    AST *i = a->down;
    while (i != NULL && i->right != NULL) {
        cout << s + "  \\__";
        ASTPrintIndent(i, s + "  |" + string(i->kind.size() + i->text.size(), ' '));
        i = i->right;
    }

    if (i != NULL) {
        cout << s + "  \\__";
        ASTPrintIndent(i, s + "   " + string(i->kind.size() + i->text.size(), ' '));
        i = i->right;
    }
}

void ASTPrint(AST *a) {
    while (a != NULL) {
        cout << " ";
        ASTPrintIndent(a, "");
        a = a->right;
    }
    cout << endl;
}

void commandPrint(AST *a) {
    if (a != NULL){
        if (a->kind == "list"){
            cout << "Seq [";

            AST* pntr = a->down;

            while(pntr != NULL){
                if(pntr->kind == ":="){
                    cout << "Assign \"" << pntr->down->kind << "\" "
                } else if(pntr->kind == "INPUT"){
                    cout << "Input \"" << pntr->down->kind << "\"";
                } else if(pntr->kind == "PRINT"){
                    cout << "Print \"" << pntr->down->kind << "\"";
                }
                if(pntr->right) cout << ", ";
                pntr = pntr->right;
            }

            cout << "]" << endl;
        }
    }
}

int main() {
    root = NULL;
    ANTLR(lang(&root), stdin);
    ASTPrint(root);
    commandPrint(root);
}


>>

#lexclass START

#token AND "AND"
#token AS "\:\="
#token DD "\:"
#token DO "DO"
#token ELSE "ELSE"
#token EMPTY "EMPTY"
#token END "END"
#token EQ "\="
#token GTH ">"
#token IF "IF"
#token INPUT "INPUT"
#token MIN "\-"
#token NOT "NOT"
#token OR "OR"
#token PLU "\+"
#token POP "POP"
#token PRINT "PRINT"
#token PUSH "PUSH"
#token SIZE "SIZE"
#token SPACE "[\ \n]" << zzskip();>>
#token TIM "\*"
#token THEN "THEN"
#token WHILE "WHILE"

// TODO: IDs/NUMs compatibles amb coma flotant

#token NUM "[0-9]+"
#token ID "[a-zA-Z][a-zA-Z0-9]*"


asig: ID AS^ nexpr;
inp: INPUT^ ID;
prnt: PRINT^ ID;
empt: EMPTY^ ID;
psh: PUSH^ ID (NUM | ID);
pop: POP^ ID ID;
siz: SIZE^ ID ID;

nexpr: (ID | NUM) ((PLU^ | MIN^ | TIM^) nexpr | );
bsub: nexpr (GTH^ | EQ^) nexpr ((AND^ | OR^) bexpr | );
bexpr: (NOT^ | ) bsub;

smpl: (asig | inp | prnt | empt | psh | pop | siz);

loop: WHILE^ bexpr DO! lang END!;
bcons: IF^ bexpr THEN! lang (ELSE! lang | ) END!;

lang: (smpl | bcons | loop)* <<#0=createASTlist(_sibling);>>;