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
  ${HDF5_INCLUDE_DIR_FORTRAN}
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
  hdf5_fortran
  hdf5_f90cstub
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

IF (WIN32 AND MSVC)
  SET_TARGET_PROPERTIES (H5FDdsm_fortran
    PROPERTIES
      LINKER_LANGUAGE Fortran
	  LINK_FLAGS "/SUBSYSTEM:CONSOLE"
	)
ENDIF (WIN32 AND MSVC)

SET_TARGET_PROPERTIES (H5FDdsm_fortran
  PROPERTIES
    LINKER_LANGUAGE Fortran
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

  IF (WIN32)
    IF (MSVC)
      IF (BUILD_SHARED_LIBS)
        SET_TARGET_PROPERTIES (H5FDdsmFortranTest
            PROPERTIES
                COMPILE_FLAGS "/MT"
        )
      ENDIF (BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (H5FDdsmFortranTest
          PROPERTIES
              LINK_FLAGS "/SUBSYSTEM:CONSOLE"
      )
    ENDIF (MSVC)
  ENDIF (WIN32)
  SET_TARGET_PROPERTIES (H5FDdsmFortranTest PROPERTIES LINKER_LANGUAGE Fortran)
 
  TARGET_LINK_LIBRARIES(H5FDdsmFortranTest
    H5FDdsm_fortran
    ${MPI_FORTRAN_LIBRARY}
 )
    
ENDIF (BUILD_TESTING)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
INSTALL (
  TARGETS
    H5FDdsm_fortran
    H5FDdsm_f90CStub
  EXPORT
    ${H5FD_DSM_EXPORTED_TARGETS}
  LIBRARY DESTINATION lib COMPONENT fortlibraries
  ARCHIVE DESTINATION lib COMPONENT fortlibraries
  RUNTIME DESTINATION bin COMPONENT fortlibraries
)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
INSTALL (
  FILES 
    ${CMAKE_CURRENT_SOURCE_DIR}/H5f90dsmproto.h 
  DESTINATION 
    include/fortran 
  COMPONENT 
    fortheaders
)

INSTALL (
  DIRECTORY
    ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/
  DESTINATION
    include/fortran
  COMPONENT
    fortheaders
)
