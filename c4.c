// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>

char *p, *lp, // current position in source code
     *data;   // data/bss pointer

int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// tokens and classes (operators last and in precedence order)
/*

Id = identifier 用户定义的identifier

//运算符
Assign  =
Cond    ?
Lor     ||
Lan     &&
Or      |
Xor     ^
And     &
Eq      ==
Ne      !=
Lt      <
Gt      >
Le      <=
Ge      >=
Shl     <<
Shr     >>
Add     +
Sub     -
Mul     *
Div     /
Mod     %
Inc     ++
Dec     --
Brak    [
*/
/*
id[Class] =
Num  常量数值
Fun 函数
Sys 系统调用
Glo 全局变量
Loc 局部变量
*/
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// opcodes
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT };

// types
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
//因为作者没有实现结构体,所以[id]指向的空间被分割为Idsz大小的块(模拟结构体)
//当id指向块首时,id[0] == id[Tk] 访问的就是Tk成员的数据(一般是指针)
//Name 指向的是这个identifier的Name
//Type 为数据类型(比如返回值类型),如CHAR,INT,INT+PTR
//Class 为类型,如Num(常量数值),Fun(函数),Sys(系统调用),Glo全局变量,Loc 局部变量
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

//词法分析
void next()
{
  char *pp;
  // 用循环来忽略空白字符,不过不能被词法分析器识别的字符都被认为是空白字符 比如 '@', '$'
  while (tk = *p) {
    ++p;
    if (tk == '\n') {
      if (src) { //命令行指明-s参数,输出源代码和对应字节码
        printf("%d: %.*s", line, p - lp, lp);
        lp = p;
        while (le < e) {
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n"); //ADJ之前的指令均有操作数
        }
      }
      ++line;
    }
    else if (tk == '#') {//#作为单行注释符号,处理#include等情况
      while (*p != 0 && *p != '\n') ++p;
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      pp = p - 1; //因为有++p,pp回退一个字符,pp指向 [这个符号] 的首字母
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++;//计算哈希值,tk起始时已经等于*p,从p开始正确
      tk = (tk << 6) + (p - pp); //哈希值加上[符号长度]
      id = sym;
      while (id[Tk]) {
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) {
          //找到同名,则tk = id[Tk] (将id看做结构体,访问其Tk成员,解释见上)
          tk = id[Tk];
          return;
        }
        id = id + Idsz;//继续循环identifier表
      }
      //找不到,发现新identifier,此时id已经指向新的identifier结构
      id[Name] = (int)pp;
      id[Hash] = tk;//哈希值
      tk = id[Tk] = Id; //token 类型为identifier
      return;
    }
    else if (tk >= '0' && tk <= '9') {// 第一位为数字,认为是数值
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }//第一位不为0,认为是十进制数
      else if (*p == 'x' || *p == 'X') {//第一位为0,且以x开头,认为是16进制数
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }//认为是八进制数
      tk = Num;//token 为数值型,返回
      return;
    }
    else if (tk == '/') {
      if (*p == '/') {//两个'/'开头,单行注释
        ++p;
        while (*p != 0 && *p != '\n') ++p;
      }
      else {
        tk = Div; //除法
        return;
      }
    }
    else if (tk == '\'' || tk == '"') {//引号开头,认为是字符(串)
      pp = data;
      while (*p != 0 && *p != tk) {//直到找到匹配的引号为止;p == 0 是什么情况没看懂;
        if ((ival = *p++) == '\\') {
          if ((ival = *p++) == 'n') ival = '\n';// '\n' 认为是'\n' 其他直接忽略'\'转义
        }
        if (tk == '"') *data++ = ival;//如果是双引号,认为是字符串,向data拷贝字符
      }
      ++p;
      if (tk == '"') ival = (int)pp; else tk = Num;//双引号则ival指向data中字符串开始,单引号则认为是数字
      return;
    }
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }//等于,赋值
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }//加,自增
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }//减,自减
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }//不等于
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }// <,<=, <<
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }//>,>= >>
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }//逻辑或,或
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;//不做处理
  }
}
//表达式分析
//lev表示运算符,因为各个运算符token是按照优先级生序排列的,所以lev大表示优先级高
void expr(int lev)
{
  int t, *d;

  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; } //直接取立即数作为表达式值
  else if (tk == '"') { //字符串
    *++e = IMM; *++e = ival; next();
    while (tk == '"') next();//连续的'"' 处理C风格多行文本 比如["abc" "def"]
    data = (char *)((int)data + sizeof(int) & -sizeof(int));//字节对齐到int
    ty = PTR;
  }
  else if (tk == Sizeof) {
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
    while (tk == Mul) { next(); ty = ty + PTR; }//多级指针,每多一级加PTR
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);//除了char是一字节,int和多级指针都是int大小
    ty = INT;
  }
  else if (tk == Id) {//identifier
    d = id; next();
    if (tk == '(') {//函数
      next();
      t = 0;//形参个数
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }//计算实参的值，压栈(传参)
      next();
      if (d[Class] == Sys) *++e = d[Val]; //系统调用,如malloc,memset,d[val]为opcode
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; } //用户定义函数,d[Val]为函数入口地址
      else { printf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; }//因为用栈传参,调整栈
      ty = d[Type];//函数返回值类型
    }
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }//d[Class] == Num,处理枚举(只有枚举是Class==Num)
    else {//变量
      //变量先取地址然后再LC/LI
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }//取地址,d[Val]是局部变量偏移量
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }//取地址,d[Val] 是全局变量指针
      else { printf("%d: undefined variable\n", line); exit(-1); }
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
    }
  }
  else if (tk == '(') {
    next();
    if (tk == Int || tk == Char) {//强制类型转换
      t = (tk == Int) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }//指针
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc); //高优先级
      ty = t;
    }
    else { //一般语法括号
      expr(Assign);
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  else if (tk == Mul) {//取指针指向值
    next(); expr(Inc);//高优先级
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    *++e = (ty == CHAR) ? LC : LI;
  }
  else if (tk == And) {//&,取地址操作
    next(); expr(Inc);
    if (*e == LC || *e == LI) --e;//根据上面的代码,token为变量时都是先取地址再LI/LC,所以--e就变成了取地址到a
    else { printf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
  }
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }//!x相当于 x==0
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }//~x 相当于x ^ -1
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  else if (tk == Sub) {
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } //数值,取负
    else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }//乘-1
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) {//处理++x,--x//x--,x++在后面处理
    t = tk; next(); expr(Inc);
    //处理++x,--x
    if (*e == LC) { *e = PSH; *++e = LC; }//地址压栈(下面SC/SI用到),再取数
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;//将数值压栈
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);//指针则加减一字,否则加减1
    *++e = (t == Inc) ? ADD : SUB;//运算
    *++e = (ty == CHAR) ? SC : SI;//存回变量
  }
  else { printf("%d: bad expression\n", line); exit(-1); }

  //爬山法
  //tk为ASCII码的都不会超过Num=128
  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    t = ty;//ty在递归过程中可能会改变,所以备份当前处理的表达式类型
    if (tk == Assign) { //赋值
      next();
      if (*e == LC || *e == LI) *e = PSH; //左边被tk=Id中变量部分处理过了,将地址压栈
      else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;//取得右值expr的值,作为a=expr的结果
    }
    else if (tk == Cond) {//x?a:b和if类似,除了不能没有else
      next();
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (int)(e + 1);
    }
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }//短路,逻辑Or运算符左边为true则表达式为true,不用计算运算符右侧的值
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }//短路,逻辑And,同上
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }//将当前值Push,计算运算符右边值,再与当前值(在栈中)做运算;
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }//expr中lev指明递归函数中最结合性不得低于哪一个运算符
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    else if (tk == Add) {
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }//处理指针
      *++e = ADD;
    }
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }//指针相减
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }//指针减数值
      else *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    else if (tk == Inc || tk == Dec) {//处理x++,x--
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB;//先自增/自减
      *++e = (ty == CHAR) ? SC : SI;//存到内存里
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD;//再相反操作,保证后自增/自减不影响这次表达式的求值
      //PS:我终于知道哪些a=1;b=a+++a++为啥等于3了
      next();
    }
    else if (tk == Brak) {//数组下标
      next(); *++e = PSH; expr(Assign);//保存数组指针, 计算下标
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  } //t==PTR时是Char,Char = 0
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      *++e = ADD;
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}

//分析函数中除了声明之外的部分,语法分析
void stmt()
{
  int *a, *b;

  if (tk == If) {
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; //branch if zero
    b = ++e;//branch address pointer
    stmt(); // 继续分析
    if (tk == Else) {
      *b = (int)(e + 3); // e + 3 位置是else 起始位置
      *++e = JMP; // if 语句 else 之前插入 JMP 跳过Else 部分
      b = ++e; // JMP目标
      next();
      stmt();//分析else 部分
    }
    *b = (int)(e + 1);//if 语句结束,无论是if BZ 跳转目标还是 else 之前的JMP的跳转目标
  }
  else if (tk == While) {//循环
    next();
    a = e + 1; // While 循环体起始地址
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;//b = While 语句结束后地址
    stmt();//处理While 语句体
    *++e = JMP; *++e = (int)a;//无条件跳转到While语句开始(包括循环条件的代码),实现循环
    *b = (int)(e + 1);//BZ跳转目标(循环结束)
  }
  else if (tk == Return) {
    next();
    if (tk != ';') expr(Assign);//计算返回值
    *++e = LEV;
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }//What's this?
  }
  else if (tk == '{') {//复合语句
    next();
    while (tk != '}') stmt();
    next();
  }
  else if (tk == ';') {
    next();
  }
  else {
    expr(Assign);//一般的语句认为是赋值语句/表达式
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

int main(int argc, char **argv)
{
  //fd file descriptor 文件描述
  //bt basetype
  //poolsz 一系列池的大小
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle; // vm registers 虚拟器的寄存器
  int i, *t; // temps

  --argc; ++argv;
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }//打开文件

  poolsz = 256*1024; // arbitrary size
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }//符号表
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }//// current position in emitted code
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }//数据段
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }//栈

  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  // 用词法分析器先把这些关键词放进符号表
  p = "char else enum if int return sizeof while "
      "open read close printf malloc memset memcmp exit void main";
  // 把关键词加进去,id[Tk]修改为和Enum一致
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  //把[库]里定义的符号(系统函数等) 加进去 Class 赋值为Sys
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  // void 认为是char
  next(); id[Tk] = Char; // handle void type
  // 记录main函数的符号id
  next(); idmain = id; // keep track of main

  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0;//字符串结尾置0
  close(fd);

  // parse declarations
  line = 1;
  next();
  while (tk) {
    bt = INT; // basetype
    if (tk == Int) next(); //已经有bt == INT
    else if (tk == Char) { next(); bt = CHAR; }//char 变量;
    else if (tk == Enum) {
      next();
      if (tk != '{') next(); // 似乎忽略了枚举名,例如 enum xxx{}
      if (tk == '{') {
        next();
        i = 0; //Enum 默认从0 开始
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; } //不是Identifier 就出错
          next();
          if (tk == Assign) { // 发现赋值语句 如 enum { Num = 128 };
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          //id 已经由 next 函数处理过
          id[Class] = Num; id[Type] = INT; id[Val] = i++;
          if (tk == ',') next();
        }
        next();
      }
    }
    //Enum 处理完tk == ';', 略过下面
    while (tk != ';' && tk != '}') {
      ty = bt;//type 类型
      while (tk == Mul) { next(); ty = ty + PTR; } // tk == Mul 表示已*开头,为指针类型,类型加PTR表示何种类型的指针
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; } //重复全局变量定义,解释见后
      next();
      id[Type] = ty; //赋值类型
      if (tk == '(') { // 函数
        id[Class] = Fun;//类型为函数型
        id[Val] = (int)(e + 1); //函数指针? 在字节码中的偏移量/地址
        next(); i = 0;
        while (tk != ')') {//参数列表
          ty = INT;
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; } //函数参数是局部变量
          //备份符号信息,要进入函数上下文了
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;//局部变量编号
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i; //局部变量偏移量
        next();
        while (tk == Int || tk == Char) { //函数内变量声明
          bt = (tk == Int) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }//处理指针型
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            //备份符号信息
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i; //存储变量偏移量
            next();
            if (tk == ',') next();
          }
          next();
        }
        *++e = ENT; *++e = i - loc;//函数局部变量数目
        while (tk != '}') stmt();//语法分析?
        *++e = LEV;//函数返回
        id = sym; // unwind symbol table locals
        while (id[Tk]) {
          //恢复符号信息
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else {
        id[Class] = Glo;//全局变量
        id[Val] = (int)data;//给全局变量在data段分配内存
        data = data + sizeof(int);
      }
      if (tk == ',') next();
    }
    next();
  }

  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
  if (src) return 0;

  // setup stack
  sp = (int *)((int)sp + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = argc;
  *--sp = (int)argv;
  *--sp = (int)t;

  // run...
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle;
    if (debug) {
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
    else if (i == IMM) a = *pc++;                                         // load global address or immediate
    else if (i == JMP) pc = (int *)*pc;                                   // jump
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    else if (i == LI)  a = *(int *)a;                                     // load int
    else if (i == LC)  a = *(char *)a;                                    // load char
    else if (i == SI)  *(int *)*sp++ = a;                                 // store int
    else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
    else if (i == PSH) *--sp = a;                                         // push

    else if (i == OR)  a = *sp++ |  a;
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;

    else if (i == OPEN) a = open((char *)sp[1], *sp);
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
    else if (i == CLOS) a = close(*sp);
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = (int)malloc(*sp);
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
  }
}
