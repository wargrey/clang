#lang stdc/c

// A line comment


/**
 * A block comment
 */
int main(int argc, char* argv[]) {
    int \u4f60\u597d = 100;     // 你好
    int \U0000597d = 50;        // 好
    
    int \U0001f60a\U0001f601 = \U00004f60\u597d + \U0000597d; // 😊😁
    
    // Output：你好😊😁, 150
    printf("\u4f60\u597d\U0001f60a\U0001f601, %d\n", \U0001f60a\U0001f601);

    return 0;
}

//
