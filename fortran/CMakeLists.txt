cmake_minimum_required (VERSION 2.8)
PROJECT (H5FDdsm_F90 C CXX Fortran)

#-----------------------------------------------------------------------------
# Add debug information to default flags (intel Fortran) : JB
#-----------------------------------------------------------------------------
IF(CMAKE_Fortran_COMPILER MATCHES ifort)
  IF (WIN32 AND MSVC)
    set(CMAKE_Fortran_FLAGS_DEBUG "/debug:full /dbglibs" CACHE 
      "Default debug compiler flags" STRING FORCE)
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "/DEBUG" CACHE 
      "Default debug linker flags" STRING FORCE)
      MESSAGE("Adding ifort flags")
  ENDIF (WIN32 AND MSVC)
ENDIF(CMAKE_Fortran_COMPILER MATCHES ifort)

#-----------------------------------------------------------------------------
# Set Include directories for headers, generated files, modules
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES( 
  ${H5FDdsm_SOURCE_DIR}
  ${CMAKE_Fortran_MODULE_DIRECTORY}
  ${HDF5_INCLUDE_DIR}/fortran
  ${H5FDdsm_F90_BINARY_DIR}
 )

#-----------------------------------------------------------------------------
# Build the C/Fortran bindings library
#-----------------------------------------------------------------------------
SET(H5FDdsm_F90_C_STUB_SRCS 
  H5FDdsmf.c
)

ADD_LIBRARY(H5FDdsm_f90CStub STATIC
  ${H5FDdsm_F90_C_STUB_SRCS}
)

TARGET_LINK_LIBRARIES(H5FDdsm_f90CStub
  H5FDdsm 
  "C:/Program Files/HDF5/lib/libhdf5_fortran_D.lib"
  "C:/Program Files/HDF5/lib/libhdf5_f90CStub_D.lib"
  ${HDF5_LIBRARY} 
  ${MPI_LIBRARY}
)

#-----------------------------------------------------------------------------
# Build the Fortran library
#-----------------------------------------------------------------------------
SET(H5FDdsm_FORTRAN_SRC
  H5FDdsmff.f90
)

ADD_LIBRARY(H5FDdsm_fortran
  ${H5FDdsm_FORTRAN_SRC}
)

TARGET_LINK_LIBRARIES(H5FDdsm_fortran
  H5FDdsm_f90CStub 
)

#-----------------------------------------------------------------------------
# Build the Fortran Tests
#-----------------------------------------------------------------------------
IF (BUILD_TESTING)
  SET(H5FDdsm_FORTRAN_TEST_SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/Testing/dsmexample.f90
  )

  ADD_EXECUTABLE(H5FDdsmFortranTest
    ${H5FDdsm_FORTRAN_TEST_SRC}
  )
    
  IF (WIN32 AND MSVC)
    set_target_properties(H5FDdsmFortranTest 
      PROPERTIES LINK_FLAGS 
#        "/NODEFAULTLIB:\"MSVCRTD.lib\" /NODEFAULTLIB:\"LIBCMTD.lib\"  /SUBSYSTEM:CONSOLE"
""
    )
#    SET_TARGET_PROPERTIES (H5FDdsmFortranTest PROPERTIES OUTPUT_NAME "H5FDdsmFortranTest")
#    SET_TARGET_PROPERTIES (H5FDdsmFortranTest PROPERTIES PREFIX "dll")
#    TARGET_LINK_LIBRARIES (H5FDdsmFortranTest "ws2_32.lib")    
  ENDIF (WIN32 AND MSVC)
  
  TARGET_LINK_LIBRARIES(H5FDdsmFortranTest
    H5FDdsm_fortran 
    "C:/Program Files/MPICH2/lib/fmpich2.lib" 
 )
    
ENDIF (BUILD_TESTING)