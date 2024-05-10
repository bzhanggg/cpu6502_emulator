cmake_minimum_required(VERSION 3.25)
project(cpu6502_emulator)

set(CMAKE_CXX_STANDARD 20)

add_executable(cpu6502_emulator "Source Files/main.cpp" "Header Files/Bus.h" "Header Files/olc6502.h" "Source Files/Bus.cpp" "Source Files/olc6502.cpp")
