#lang stdc/c

#include <stdio.h>

// A line comment

static char* \u22error = "error";
static char* _\u597d_ = "robust test";
static char* error = "bad \u22 string";
static char* octal = "'\1111' is an 'I' followed by an '1'";

/**
 * A block comment
 */
int main(int argc, char* argv[]) {
    int \u4f60\u597d = 100;     // 你好
    int \U0000597d = 50;        // 好
    
    int \U0001f60a\U0001f601 = \U00004f60\u597d + \U0000597d; // 😊😁
    
    // printf：你好😊😁, 150
    printf("\u4f60\x597d\U0001f60a\U0001f601\054 %d\n", \U0001f60a\U0001f601);

    return 0;
}
