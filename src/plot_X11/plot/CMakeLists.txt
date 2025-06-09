include_directories("${CMAKE_SOURCE_DIR}/src/plot_X11/plot")
add_library(plot STATIC axdec.f rfform.f angplt.f modplt.f xaxlog.f numdec.f yaxlin.f 
                        simula.f blanks.f smootl.f vclip.f input.f extleg.f redata.f out_tl.f 
                        restore.f decode.f plotfip.f initplt.f multplt.f legend.f extn.f scissor.f 
                        proflr.f seisl.f)

add_executable(fipplot fipplot.f)
target_link_libraries(fipplot PRIVATE plot mindis ${X11_LIBRARIES})
target_include_directories(fipplot PRIVATE ${X11_INCLUDE_DIR})

install(TARGETS fipplot DESTINATION) 

