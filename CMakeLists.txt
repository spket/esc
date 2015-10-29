project(esc)
cmake_minimum_required(VERSION 2.8)

set(APP_SRC_DIR ${PROJECT_SOURCE_DIR}/src)
set(APP_INC_DIR ${PROJECT_SOURCE_DIR}/src)

include_directories(${APP_INC_DIR})

if(WIN32)
	set(APP_OS win)
	add_definitions(-DUNICODE -D_UNICODE)
elseif(UNIX)
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11") #-Wall
	SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lrt")
endif()

file(GLOB_RECURSE APP_SOURCES ${APP_SRC_DIR}/*.cpp)
file(GLOB_RECURSE TP_SOURCES ${APP_SRC_DIR}/*_*.cpp)

#LIST(REMOVE_ITEM APP_SOURCES ${TP_SOURCES})

file(GLOB_RECURSE TP_SOURCES ${APP_SRC_DIR}/*_${APP_OS}.cpp)
list(APPEND APP_SOURCES ${TP_SOURCES})

if(WIN32)
	file(GLOB TP_SOURCES ${APP_SRC_DIR}/*.rc)
	list(APPEND APP_SOURCES ${TP_SOURCES})

	add_executable(esc WIN32 ${APP_SOURCES})
else()
	file(GLOB_RECURSE TP_SOURCES ${APP_SRC_DIR}/*_posix.cpp)

	list(APPEND APP_SOURCES ${TP_SOURCES})
	
	add_executable(esc ${APP_SOURCES})
endif()

#target_link_libraries(esc ${EXTRA_LIBS})
