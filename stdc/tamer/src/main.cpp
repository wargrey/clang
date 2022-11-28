#lang stdc/cpp

#include <stdio.h>  /* C 标准输入输出头文件 */

#include <iostream> /* C++ 标准输入输出头文件 */

int main(int argc, char* argv[]) {
    std::string hw = R"(Hello, World!)";
       
    /* 用 C++ 的方式输出 "Hello, World!" */
    std::cout << hw << std::endl;

    /* 或者, 用 C 的方式输出 "Hello, C!" */
    printf("Hello, C!");

    return 0;
}

