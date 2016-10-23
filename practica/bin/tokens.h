#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: lego.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define AND 2
#define AT 3
#define COMMA 4
#define DEF 5
#define EAST 6
#define ENDEF 7
#define EQ 8
#define FITS 9
#define GRID 10
#define GTHA 11
#define HEIGHT 12
#define LCLA 13
#define LPAR 14
#define LTHA 15
#define MOVE 16
#define NORTH 17
#define PLACE 18
#define POP 19
#define PUSH 20
#define RCLA 21
#define RPAR 22
#define SOUTH 23
#define SPACE 24
#define WEST 25
#define WHILE 26
#define NUM 27
#define ID 28
#define LT 29
#define GT 30
#define ASSIG 31

#ifdef __USE_PROTOS
void pos(AST**_root);
#else
extern void pos();
#endif

#ifdef __USE_PROTOS
void deffinal(AST**_root);
#else
extern void deffinal();
#endif

#ifdef __USE_PROTOS
void def(AST**_root);
#else
extern void def();
#endif

#ifdef __USE_PROTOS
void placedef(AST**_root);
#else
extern void placedef();
#endif

#ifdef __USE_PROTOS
void fits(AST**_root);
#else
extern void fits();
#endif

#ifdef __USE_PROTOS
void condWhile(AST**_root);
#else
extern void condWhile();
#endif

#ifdef __USE_PROTOS
void fitspos(AST**_root);
#else
extern void fitspos();
#endif

#ifdef __USE_PROTOS
void expr(AST**_root);
#else
extern void expr();
#endif

#ifdef __USE_PROTOS
void cond(AST**_root);
#else
extern void cond();
#endif

#ifdef __USE_PROTOS
void loopops(AST**_root);
#else
extern void loopops();
#endif

#ifdef __USE_PROTOS
void asig(AST**_root);
#else
extern void asig();
#endif

#ifdef __USE_PROTOS
void movement(AST**_root);
#else
extern void movement();
#endif

#ifdef __USE_PROTOS
void loop(AST**_root);
#else
extern void loop();
#endif

#ifdef __USE_PROTOS
void height(AST**_root);
#else
extern void height();
#endif

#ifdef __USE_PROTOS
void definition(AST**_root);
#else
extern void definition();
#endif

#ifdef __USE_PROTOS
void grid(AST**_root);
#else
extern void grid();
#endif

#ifdef __USE_PROTOS
void ops(AST**_root);
#else
extern void ops();
#endif

#ifdef __USE_PROTOS
void defs(AST**_root);
#else
extern void defs();
#endif

#ifdef __USE_PROTOS
void lego(AST**_root);
#else
extern void lego();
#endif

#endif
extern SetWordType zzerr1[];
extern SetWordType zzerr2[];
extern SetWordType zzerr3[];
extern SetWordType zzerr4[];
extern SetWordType zzerr5[];
extern SetWordType zzerr6[];
extern SetWordType setwd1[];
extern SetWordType zzerr7[];
extern SetWordType zzerr8[];
extern SetWordType zzerr9[];
extern SetWordType setwd2[];
extern SetWordType setwd3[];
