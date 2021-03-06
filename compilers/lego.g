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

typedef struct {
    int x, y, z;
    int h, w;
} tblock;

bool operator==(const tblock &b1, const tblock &b2) {
    return b1.x == b2.x and b1.y == b2.y and b1.z == b2.z and b1.h == b2.h and b1.w == b2.w;
}

typedef struct {
    int n, m;
    vector<vector<int> > height;
    map<string, tblock> blocks;
} Graella;

typedef map<string, tblock>::iterator blockIt;
typedef pair<vector<pair<string, tblock> >, vector<string> > longpair;

// Global definitions
Graella g;
map<string, AST *> functions;
int currentUnnamed = 0;

// Function header (in order to make things work)
void executeLevel(AST *a);


// Helper functions

int parseKind(string s) {
    return atoi(s.c_str());
}

int blockHeight(string id) {
    blockIt it = g.blocks.find(id);
    if (it == g.blocks.end()) {
        return -1;
    } else {
        tblock b = g.blocks[id];
        int height = 0;
        for (int i = 0; i < b.w; i++) {
            for (int j = 0; j < b.h; j++) {
                height = max(height, g.height[b.y + j][b.x + i]);
            }
        }
        return height;
    }
}

pair<bool, int> parseCondInt(AST *pntr) {
    if (pntr->kind == "HEIGHT") {
        string id = pntr->down->kind;
        int height = blockHeight(id);
        return {height > -1, height};
    } else {
        return {true, parseKind(pntr->kind)};
    }
}

bool isUniform(int x, int y, int h, int w) {
    if (x < 0) return false;
    if (y < 0) return false;
    if (x + w > g.m) return false;
    if (y + h > g.n) return false;

    int init = g.height[y][x];
    for (int i = 0; i < w; i++) {
        for (int j = 0; j < h; j++) {
            if (g.height[y + j][x + i] != init) return false;
        }
    }

    return true;
}

pair<int, int> firstAvailable(string id, int h, int w, int lev) {
    tblock b = g.blocks[id];
    int max_lev = blockHeight(id);
    int n = b.x + b.w;
    int m = b.y + b.h;

    if (lev == -1) lev = 1;
    else max_lev = lev;

    for (int l = lev; l <= max_lev; l++) {
        int y = b.y;
        while (y < m) {
            int i = b.x;
            int j = y;
            while (j >= b.y and i < n) {
                if (g.height[j][i] == l && isUniform(i, j, h, w)) return {i, j};
                i++;
                j--;
            }
            y++;
        }

        int x = b.x + 1;
        while (x < n) {
            int i = x;
            int j = m - 1;
            while (i < n) {
                if (g.height[j][i] == l && isUniform(i, j, h, w)) return {i, j};
                i++;
                j--;
            }
            x++;
        }
    }

    return {-1, -1};
}

pair<int, int> firstAvailable(string id, int h, int w) {
    return firstAvailable(id, h, w, -1);
}

tblock calculateMovement(string dir, int i, tblock b) {
    if (dir == "NORTH") b.y -= i;
    else if (dir == "SOUTH") b.y += i;
    else if (dir == "EAST") b.x += i;
    else if (dir == "WEST") b.x -= i;
    return b;
}

longpair blocksAbove(string id) {
    longpair toReturn;
    tblock b = g.blocks[id];
    for (map<string, tblock>::iterator it = g.blocks.begin(); it != g.blocks.end(); it++) {
        if (it->first != id) {
            if (it->second.z > b.z) {
                if (it->second.x >= b.x && it->second.x <= b.x + b.w && it->second.y >= b.y &&
                    it->second.y <= b.y + b.h) {
                    toReturn.first.push_back({it->first, it->second});
                }
            } else if (it->second == b) {
                toReturn.second.push_back(it->first);
            }
        }
    }
    return toReturn;
}

tblock insertBlock(tblock b) {
    for (int i = 0; i < b.w; i++) {
        for (int j = 0; j < b.h; j++) {
            g.height[b.y + j][b.x + i] += 1;
        }
    }
    b.z = g.height[b.y][b.x];
    return b;
}

tblock insertBlock(int x, int y, int h, int w) {
    tblock b = {x, y, 0, h, w};
    return insertBlock(b);
}

void removeBlock(string id) {
    tblock b = g.blocks[id];
    for (int i = 0; i < b.w; i++) {
        for (int j = 0; j < b.h; j++) {
            g.height[b.y + j][b.x + i] -= 1;
        }
    }
    g.blocks.erase(id);
}

string recPush(AST *pntr, string id, int rec) {
    string lId, rId;
    int h, w;

    if (pntr->down->right->kind == "PUSH" or pntr->down->right->kind == "POP") {
        rId = recPush(pntr->down->right, id, rec + 1);
    } else {
        rId = pntr->down->right->kind;
    }

    if (rId == "done") return "done";
    else if (rId == "err") return "err";

    blockIt itR = g.blocks.find(rId);
    if (itR == g.blocks.end()) return "err";

    tblock b;
    longpair above;

    if (pntr->down->kind == "list") {
        w = parseKind(pntr->down->down->kind);
        h = parseKind(pntr->down->down->right->kind);
    } else {
        lId = pntr->down->kind;
        blockIt itL = g.blocks.find(lId);
        if (itL == g.blocks.end()) return "err";
        b = itL->second;
        w = b.w;
        h = b.h;
    }

    if (pntr->kind == "PUSH") {
        if (lId.length() > 0) {
            above = blocksAbove(lId);
            for (int a = 0; a < above.first.size(); a++)
                removeBlock(above.first[a].first);
            removeBlock(lId);
        }
        pair<int, int> fa = firstAvailable(rId, h, w);
        int x = fa.first;
        int y = fa.second;
        if (x != -1 and y != -1) {
            if (lId.length() > 0) {
                int mov_x = x - b.x;
                int mov_y = y - b.y;
                g.blocks[lId] = insertBlock(x, y, h, w);
                for (int a = 0; a < above.first.size(); a++) {
                    tblock temp = above.first[a].second;
                    temp.x += mov_x;
                    temp.y += mov_y;
                    removeBlock(above.first[a].first);
                    g.blocks[above.first[a].first] = insertBlock(temp);
                }
                for (int a = 0; a < above.second.size(); a++) {
                    g.blocks[above.second[a]] = g.blocks[lId];
                }
            } else {
                g.blocks["U" + to_string(currentUnnamed++)] = insertBlock(x, y, h, w);
            }
            if (rec == 0 && id != rId) {
                g.blocks[id] = g.blocks[rId];
            }
            return rId;
        } else {
            if (lId.length() > 0) {
                g.blocks[lId] = insertBlock(b);
                for (int a = 0; a < above.first.size(); a++) {
                    g.blocks[above.first[a].first] = insertBlock(above.first[a].second);
                }
            }
            return "err";
        }
    } else if (pntr->kind == "POP" and lId.length() > 0) {
        above = blocksAbove(lId);
        for (int a = 0; a < above.first.size(); a++) {
            removeBlock(above.first[a].first);
        }
        for (int a = 0; a < above.second.size(); a++) {
            g.blocks.erase(above.second[a]);
        }
        removeBlock(lId);
        return rId;
    }

    return "done";
}

bool evaluateCondition(AST *pntr) {
    string oper = pntr->kind;
    if (oper == "FITS") {
        string id = pntr->down->kind;
        blockIt it = g.blocks.find(id);
        if (it == g.blocks.end()) {
            cout << "Block " << id << " does not exist." << endl;
            return false;
        } else {
            int w = parseKind(pntr->down->right->down->kind);
            int h = parseKind(pntr->down->right->down->right->kind);
            int lev = parseKind(pntr->down->right->right->kind) - 1;
            pair<int, int> fa = firstAvailable(id, h, w, lev);
            return fa.first != -1 and fa.second != -1;
        }
    } else {
        pair<bool, int> c1 = parseCondInt(pntr->down);
        pair<bool, int> c2 = parseCondInt(pntr->down->right);
        if (c1.first && c2.first) {
            if (oper == ">") return c1.second > c2.second;
            else return c1.second < c2.second;
        } else {
            cout << "Invalid condition." << endl;
            return false;
        }
    }
}

// Command functions

void executePlace(AST *pntr, string id) {
    int w = parseKind(pntr->down->down->kind);
    int h = parseKind(pntr->down->down->right->kind);
    int x = parseKind(pntr->down->right->down->kind) - 1;
    int y = parseKind(pntr->down->right->down->right->kind) - 1;

    if (y > g.n - 1 or x > g.m - 1) {
        cout << "Cannot PLACE a block outside the grid." << endl;
    } else if (g.height[y][x] > 0) {
        cout << "Cannot PLACE a block where there is another block. Use PUSH instead." << endl;
    } else if (w < 1 and h < 1) {
        cout << "Cannot PLACE a block with height and/or width less than 1." << endl;
    } else {
        if (isUniform(x, y, h, w)) {
            removeBlock(id);
            g.blocks[id] = insertBlock(x, y, h, w);
        } else {
            cout << "PLACE operation not allowed." << endl;
        }
    }
}

void executeMove(AST *pntr) {
    string id = pntr->down->kind;
    blockIt it = g.blocks.find(id);

    if (it == g.blocks.end()) {
        cout << "Cannot MOVE block " << id << " because does not exist." << endl;
    } else {
        string dir = pntr->down->right->kind;
        int i = parseKind(pntr->down->right->right->kind);
        tblock b = it->second;
        tblock newBlock = calculateMovement(dir, i, b);
        longpair above = blocksAbove(id);
        for (int a = 0; a < above.first.size(); a++) {
            removeBlock(above.first[a].first);
        }
        removeBlock(id);

        if (isUniform(newBlock.x, newBlock.y, newBlock.h, newBlock.w)) {
            g.blocks[id] = insertBlock(newBlock);
            for (int a = 0; a < above.first.size(); a++) {
                tblock temp = calculateMovement(dir, i, above.first[a].second);
                removeBlock(above.first[a].first);
                g.blocks[above.first[a].first] = insertBlock(temp);
            }
            for (int a = 0; a < above.second.size(); a++) {
                g.blocks[above.second[a]] = g.blocks[id];
            }
        } else {
            g.blocks[id] = insertBlock(b);
            for (int a = 0; a < above.first.size(); a++) {
                g.blocks[above.first[a].first] = insertBlock(above.first[a].second);
            }
            cout << "MOVE not allowed." << endl;
        }
    }
}

void executePush(AST *pntr, string id) {
    string res = recPush(pntr, id, 0);
    if (res == "err") {
        cout << "PUSH operation not allowed." << endl;
    }
}

void executeWhile(AST *pntr) {
    if (pntr->down->kind == "AND") {
        while (evaluateCondition(pntr->down->down) && evaluateCondition(pntr->down->down->right)) {
            executeLevel(pntr->down->right);
        }
    } else {
        while (evaluateCondition(pntr->down)) {
            executeLevel(pntr->down->right);
        }
    }
}

void executeHeight(AST *pntr) {
    string id = pntr->down->kind;
    int height = blockHeight(id);
    if (height < 0) {
        cout << "Block " << id << " does not exist." << endl;
    } else {
        cout << "Height of block " << id << " is " << height << "." << endl;
    }
}

void executeDef(AST *pntr) {
    string name = pntr->down->kind;
    functions[name] = pntr->down->right;
}


// Main functions

void executeLevel(AST *a) {
    AST *next = a->down;

    while (next) {
        if (next->kind == "=") {
            string id = next->down->kind;
            if (next->down->right->kind == "PLACE") {
                executePlace(next->down->right, id);
            } else if (next->down->right->kind == "PUSH") {
                executePush(next->down->right, id);
            }
        } else if (next->kind == "MOVE") {
            executeMove(next);
        } else if (next->kind == "WHILE") {
            executeWhile(next);
        } else if (next->kind == "HEIGHT") {
            executeHeight(next);
        } else if (next->kind == "DEF") {
            executeDef(next);
        } else {
            string name = next->kind;
            map<string, AST *>::iterator it = functions.find(name);
            if (it == functions.end()) {
                cout << "Function " << name << " does not exist." << endl;
            } else {
                executeLevel(functions[name]);
            }
        }
        next = next->right;
    }
}

void executeListInstructions(AST *a) {
    AST *pntr = a->down;

    // Build grid
    g.n = parseKind(pntr->down->kind);
    g.m = parseKind(pntr->down->right->kind);
    g.height = vector<vector<int> >(g.n, vector<int>(g.m, 0));

    // Reading defs
    executeLevel(pntr->right->right);

    // Executing instructions
    executeLevel(pntr->right);

    cout << endl;
}


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

void printBlocks() {
    cout << "BLOCKS:" << endl;
    for (map<string, tblock>::iterator it = g.blocks.begin(); it != g.blocks.end(); it++) {
        cout << it->first << "=> X=" << it->second.x + 1 << " Y=" << it->second.y + 1 << " Z=" << it->second.z << " W="
             << it->second.w << " H="
             << it->second.h << endl;
    }
    cout << endl;
}

void printGrid() {
    cout << "GRID (HEIGHTS):" << endl;
    for (int i = 0; i < g.n; i++) {
        for (int j = 0; j < g.m; j++) {
            cout << setfill('0') << setw(2) << g.height[i][j] << " ";
        }
        cout << endl;
    }
    cout << endl;
}


int main() {
    root = NULL;
    ANTLR(lego(&root), stdin);
    //ASTPrint(root);
    executeListInstructions(root);
    printBlocks();
    printGrid();
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
