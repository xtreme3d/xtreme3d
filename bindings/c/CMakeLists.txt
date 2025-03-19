cmake_minimum_required(VERSION 3.0)
project(xtreme3d_demo)

file(GLOB_RECURSE sources src/*.c src/*.h)
add_executable(example ${sources} ${data})
target_include_directories(example PRIVATE include/)

target_link_directories(example PRIVATE lib/)
target_link_libraries(example xtreme3d.lib)
