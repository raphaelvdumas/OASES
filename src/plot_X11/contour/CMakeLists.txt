
include_directories("${CMAKE_SOURCE_DIR}/src/plot_X11/contour")

add_library(contour STATIC add.f arc.f axes.f axlen.f backbott.f batch.f bottom.f boxbw.f bw.f
                            calcxy.f chara.f conseg.f contur.f cylspr.f double.f filetype.f fileopen.f 
                            icondr.f icontr.f iconfr.f iconsv.f iexpdr.f ipareq.f
                            mainmtv.f mainmat.f mainras_min.f newgcol.f novalue.f numdec.f
                            order.f plotny.f pregrd.f probab.f rcmoden.f readpe.f redata.f 
                            sample.f simpson.f simula.f smoothc.f smootl.f star.f stardis.f statist.f
                            undef.f unit.f user.f window.f writez.f xaxlin.f xrev.f yaxlin.f yaxlog.f 
                            ylog.f ylogun.f zgrid.f zin.f zzlev.f)

add_executable(saccon main.f)
target_link_libraries(saccon PRIVATE contour mindis apmath2d ${X11_LIBRARIES})
target_include_directories(saccon PRIVATE ${X11_INCLUDE_DIR})

install(TARGETS saccon DESTINATION) 