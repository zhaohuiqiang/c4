

#c4-c in 4 functions 虚拟机分析 ##寄存器

int *pc, *sp, *bp, a, cycle; 

    pc 程序计数器/指令指针
    sp 堆栈寄存器,指向栈顶,栈由高地质向低地址生长
    bp 基址寄存器
    a 累加器
    cycle 执行指令计数

#指令集

    LEA 去局部变量地址,以[PC+1]地址,以bp为基址,地址载入累加器a
    IMM [PC+1]作为为立即数载入累加器a
    JMP 无条件跳转到[PC+1]
    JSR 进入子程序,将PC+2入栈作为返回地址,跳转到[PC+1]
    BZ 累加器为零时分支,当累加器a为0时跳转到[PC+1],不为零时跳转到PC+2继续执行
    BNZ 累加器不为零时分支,当累加器a不为0时跳转到[PC+1],为零时跳转到PC+2
    ENT 进入子程序,将bp压栈,基址bp指向栈顶,然后将栈顶生长[PC+1]字,作为参数传递空间
    ADJ
    LEV 离开子程序,堆栈指针sp = bp,从堆栈中弹出基址bp,pc
    LI 以a为地址取int数
    LC 以a为地址取char
    SI 以栈顶为地址存int数并弹栈[[sp++]]=a
    SC 以栈顶为地址存char并弹栈[[sp++]]=a
    PSH 将a压栈
    OR a = [sp++] | a
    XOR a = [sp++] ^ a
    AND a = [sp++] & a
    EQ a = [sp++] == a
    NE a = [sp++] != a
    LT a = [sp++] a
    GT a = [sp++] > a
    LE a = [sp++] <= a
    GE a = [sp++] >= a
    SHL a = [sp++] << a
    SHR a = [sp++] >> a
    ADD a = [sp++] + a
    SUB a = [sp++] - a
    MUL a = [sp++] * a
    DIV a = [sp++] / a
    MOD a = [sp++] % a
    OPEN 调用C库函数open,堆栈传递2个参数(第一个参数先进栈,返回值存入累加器a,下同)
    READ 调用C库函数read,堆栈传递2个参数
    CLOS 调用C库函数close,堆栈传递2个参数
    PRTF 调用C库函数printf,[pc+1]表明参数个数,传递至多六个参数
    MALC 调用C胡函数malloc,堆栈传递一个参数
    MSET 调用C库函数memset,堆栈传递3个参数
    MCMP 调用C库函数memcmp,堆栈传递3个参数
    EXIT 打印虚拟机执行情况,返回[sp]

