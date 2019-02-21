#include <stdio.h>

#define HE HI
#define LLO _THERE
#define HELLO "Hi There"
#define CAT(a,b) a##b
#define XCAT(a,b) CAT(a,b)

/** Order of macro expansion
 * 1. Stringification Operators
 * 2. Parameters
 * 3. Concatenation Operators
 * 4. The resulting tokens
 */

int main(int argc, const char* argv[]) {
    const char* HI_THERE = "Hi_There";

    printf("%s, %s.\n",
            CAT(HE,LLO), /* "Hi There" */
            XCAT(HE,LLO) /* HI_THERE */);

    return 0;
}

