#include <assert.h>
#include <ctype.h> 
#include <stdarg.h> 
#include <stdbool.h> 
#include <stddef.h> 
#include <stdint.h> 
#include <stdio.h> 
#include <stdlib.h> 
#include <string.h>
#include <sys/mman.h> 


static __attribute((noreturn)) void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt); 
    vfpringf(stderr, fmt, ap);
    fprintf(stderr, "\n"); 
    va_end(ap);
    exit(1);
}


// =========================================
//   Lisp objects
// =========================================


// The Lisp object type 
enum {
    // Regular objects visible from the user
    TINT = 1,
    TCELL, 
    TSYMBOL,
    TPRIMITIVE,
    TFUNCTION,
    TMACRO,
    TENV,
    // The marker that indicates the object has been moved to other location by GC. 
    // Only GC function can handle TMOVED 
    TMOVED, 
    // Const objects. They are statically allocated and will never be GC. 
    TTRUE,
    TNIL,
    TDOT,
    TCPAREN,
};




// Thpedef for the primitive function 
struct Obj; 



