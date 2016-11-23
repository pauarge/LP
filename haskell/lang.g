#header
<<
#include <string>
#include <iostream>
#include <iomanip>
#include <map>
#include <vector>
#include <cstdlib>
#include <cmath>

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

// Global definitions

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

int main() {
    root = NULL;
    ANTLR(lego(&root), stdin);
    ASTPrint(root);
}


>>

#lexclass START

#token AND "AND"
#token AT "AT"
#token COMMA "\,"
#token DEF "DEF"
#token EAST "EAST"
#token ENDEF "ENDEF"
#token EQ "\="
#token FITS "FITS"
#token GRID "Grid"
#token GTHA "\>"
#token HEIGHT "HEIGHT"
#token LCLA "\["
#token LPAR "\("
#token LTHA "\<"
#token MOVE "MOVE"
#token NORTH "NORTH"
#token PLACE "PLACE"
#token POP "POP"
#token PUSH "PUSH"
#token RCLA "\]"
#token RPAR "\)"
#token SOUTH "SOUTH"
#token SPACE "[\ \n]" << zzskip();>>
#token WEST "WEST"
#token WHILE "WHILE"

#token NUM "[0-9]+"
#token ID "[a-zA-Z][a-zA-Z0-9]*"


pos: LPAR! NUM COMMA! NUM RPAR! <<#0=createASTlist(_sibling);>>;
deffinal: ID ((POP^ | PUSH^) deffinal | );
def: (pos | ID) PUSH^ deffinal;
placedef: PLACE^ pos AT! pos;

fits: FITS^ LPAR! ID COMMA! fitspos COMMA! NUM RPAR!;
condWhile: (fits | height (LT^ | GT^ | ASSIG^) NUM) (AND^ condWhile | );
fitspos: NUM COMMA! NUM <<#0=createASTlist(_sibling);>>;

expr: (height (LTHA^ | GTHA^) NUM | fits);
cond: expr (AND^ expr)*;

loopops: asig<<#0=createASTlist(_sibling);>>;

asig: ID (EQ^ (placedef | def) | );
movement: MOVE^ ID (NORTH | SOUTH | EAST | WEST) NUM; 
loop: WHILE^ LPAR! cond RPAR! LCLA! loopops RCLA!;
height: HEIGHT^ LPAR! ID RPAR!;

definition: DEF^ ID ops ENDEF!;

grid: GRID^ NUM NUM;
ops: (asig | movement | loop | height)* <<#0=createASTlist(_sibling);>>;
defs: (definition)* <<#0=createASTlist(_sibling);>>;

lego: grid ops defs <<#0=createASTlist(_sibling);>>;
