module decs.dbg;

import core.stdc.stdio;
import core.stdc.stdarg;
import core.stdc.stdlib;

enum RESET = "\033[0m";
enum RED = "\033[1;31m";
enum GREEN = "\033[1;32m";
enum YELLOW = "\033[1;33m";
enum BLUE = "\033[1;34m";
enum PINK = "\033[1;35m";
enum CYAN = "\033[1;36m";
enum WHITE = "\033[1;37m";

pragma(printf)
extern(C) void panic(const char* fmt, ...) 
{
    printf(RED);

    va_list args;
    va_start(args, fmt);

    vprintf(fmt, args);
    va_end(args);

    printf(RESET);
    printf("\n");
    abort();
}

pragma(inline)
{
    bool is_nan(const ref float v)
    {
        //import core.stdc.math : isnan;
        //return isnan(v) != 0;
        return isNaN(v);
    }
    
    bool is_nan(const ref double v)
    {
        //import core.stdc.math : isnan;
        //return isnan(v) != 0;
        return isNaN(v);
    }

    // TODO: hmm ^^^^^^^^^^^^
    bool isNaN(X)(X x)if (__traits(isFloating, X))
    {
        version (all)
        {
            return x != x;
        }
        else
        {
            panic("not supported");
        }
    }
}