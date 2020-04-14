cmake_minimum_required(VERSION 3.15)
project(codegen_llvm)
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(CMAKE_CXX_STANDARD 14)
add_executable(codegen_llvm main.cpp)
llvm_map_components_to_libnames(llvm_libs Analysis
        Core
        ExecutionEngine
        InstCombine
        Object
        OrcJIT
        RuntimeDyld
        ScalarOpts
        Support
        native)

# Link against LLVM libraries
target_link_libraries(codegen_llvm ${llvm_libs})
