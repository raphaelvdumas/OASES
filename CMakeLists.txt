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

set(CMAKE_INSTALL_PREFIX "release/Oases-3.1-${CMAKE_SYSTEM_NAME}${NBITS}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -O3 -std=legacy -fstrength-reduce -fexpensive-optimizations -fall-intrinsics -m${NBITS}")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w -O3 -std=c11 -m${NBITS}")

#---------------------------------------------------
# Utility static libraries
#---------------------------------------------------
add_subdirectory(${CMAKE_SOURCE_DIR}/src/2d)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/3d)
add_subdirectory(${CMAKE_SOURCE_DIR}/src/pulsplot)

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

# Don't know yet how to avoid msys64 path with Ninja... Anyways, copy the required DLL on install automatically.
if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    if(RESULT GREATER -1)
        set(REQUIRED_DLLS "libgfortran-5.dll" "libgcc_s_dw2-1.dll")
    else()  
        set(REQUIRED_DLLS "libgfortran-5.dll" "libgcc_s_seh-1.dll")
    endif()
    foreach(EACH_DLL ${REQUIRED_DLLS})
        message(STATUS "Finding ${EACH_DLL} in C:/msys64/mingw${NBITS}/bin/ for copy...")    
        install(CODE "
            execute_process(
                COMMAND ${CMAKE_COMMAND} -E copy
                \"C:/msys64/mingw${NBITS}/${EACH_DLL}\" 
                \"${CMAKE_INSTALL_PREFIX}/bin/${EACH_DLL}\"
            )
        ")
    endforeach()
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(REQUIRED_SO "libgcc_s.so.1" "libgfortran.so.5")
    set(PATH_SO "/usr/lib/x86_64-linux-gnu")

    foreach(EACH_SO ${REQUIRED_SO})
        message(STATUS "Finding ${EACH_SO} in ${PATH_SO} for copy...")    
        install(CODE "
            execute_process(
                COMMAND ${CMAKE_COMMAND} -E copy
                \"${PATH_SO}/${EACH_SO}\" 
                \"${CMAKE_INSTALL_PREFIX}/bin/${EACH_SO}\"
            )
        ")
    endforeach()

    file(GLOB PLOT_SHELLS "${CMAKE_SOURCE_DIR}/misc/plot_X11/*")
    foreach(EACH_SHELL ${PLOT_SHELLS})
        get_filename_component(NAME ${EACH_SHELL} NAME)
        message(STATUS "Copying ${NAME} ...")    
        install(CODE "
            execute_process(
                COMMAND ${CMAKE_COMMAND} -E copy
                \"${CMAKE_SOURCE_DIR}/misc/plot_X11/${NAME}\" 
                \"${CMAKE_INSTALL_PREFIX}/bin/${NAME}\"
            )
        ")
    endforeach()
endif()
