include_directories("${CMAKE_SOURCE_DIR}/src/plot_X11/mindis")
add_library(mindis STATIC mindis.f mindis1.f mindis2.f mindis3.f mindis4.f mindis6.f hershey.f outidn.f gistart.c)

# mintotek executable didn't build by default on prev system.  
# if you uncomment this, you'll have to figure out the undefined references
set(MINDIS_CORE mintopost mintops) # mintotek

foreach(CORE_LIB ${MINDIS_CORE})
  add_executable(${CORE_LIB} ${CORE_LIB}.f)
  target_link_libraries(${CORE_LIB} PRIVATE mindis ${X11_LIBRARIES})
  target_include_directories(${CORE_LIB} PRIVATE ${X11_INCLUDE_DIR})
endforeach()

install(TARGETS ${MINDIS_CORE} DESTINATION) 