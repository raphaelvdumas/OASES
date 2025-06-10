cmake_minimum_required (VERSION 3.21)

project(Oases LANGUAGES C Fortran)

if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
else()
    message(FATAL_ERROR "Unsupported system name '${CMAKE_SYSTEM_NAME}'. CMake for GCC with 'Windows' or 'Linux' only.")
endif()


# Check if the string contains "86". Supposing we're in a 64 bit system to build
string(FIND "${CMAKE_C_COMPILER}" "86" RESULT)
if(RESULT GREATER -1)
    set(NBITS "32")  # Set NBITS to "32" if "86" is found
else()    
    set(NBITS "64")  # Set NBITS to "64" if "86" is not found
endif()

set(CMAKE_INSTALL_PREFIX ".")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -O3 -std=legacy -fstrength-reduce -fexpensive-optimizations -fall-intrinsics -m${NBITS}")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w -O3 -std=c11 -m${NBITS}")

#---------------------------------------------------
# Utility static libraries
#---------------------------------------------------
add_subdirectory(${CMAKE_SOURCE_DIR}/src/2d)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/3d)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/pulsplot)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/c-snap)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  find_package(X11)

  if (NOT X11_FOUND)
    message(FATAL_ERROR "X11 not found! Please install libx11-dev ('sudo apt install libx11-dev' for Debian/Ubuntu) or the appropriate X11 development package.")
  endif()

  add_subdirectory(${CMAKE_SOURCE_DIR}/src/plot_X11/mindis)
  add_subdirectory(${CMAKE_SOURCE_DIR}/src/plot_X11/plot)
  add_subdirectory(${CMAKE_SOURCE_DIR}/src/plot_X11/contour)
  message(WARNING "'Plotmtv won't be installed. Ensure that you installed it with the './third_party/Plotmtv1.4.1' folder.")
else()
  message(WARNING "'contour', 'plot' and 'mindis' depends on X11 ('Linux' only). They are not available on 'Windows'.")
endif()