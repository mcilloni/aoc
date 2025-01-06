cmake_minimum_required(VERSION 3.26.0)

project(p11 VERSION 0.0.1 LANGUAGES C)

set(CMAKE_C_STANDARD 17)

set(CMAKE_C_STANDARD_REQUIRED ON)

add_executable(p11 main.c hashtable.c)