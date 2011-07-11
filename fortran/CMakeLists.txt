cmake_minimum_required (VERSION 2.8)
PROJECT (H5FDdsm_F90 C CXX Fortran)

#-----------------------------------------------------------------------------
# Add MPI Fortran library on win32
#-----------------------------------------------------------------------------
IF (WIN32 AND CMAKE_MAJOR_VERSION EQUAL 2 AND CMAKE_MINOR_VERSION EQUAL 8
  AND CMAKE_PATCH_VERSION LESS 5)
# Now defined with CMake v2.8.5
  FIND_LIBRARY(MPI_Fortran_LIBRARIES NAMES fmpich2 mpichf90
    HINTS $ENV{ProgramFiles}/MPICH2/lib)
ENDIF (WIN32 AND CMAKE_MAJOR_VERSION EQUAL 2 AND CMAKE_MINOR_VERSION EQUAL 8
  AND CMAKE_PATCH_VERSION LESS 5)

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

IF (H5FD_DSM_BUILD_STEERING)
  SET(H5FDdsm_F90_C_STUB_SRCS
    H5FDdsmSteeringf.c
    ${H5FDdsm_F90_C_STUB_SRCS}
  )
ENDIF (H5FD_DSM_BUILD_STEERING)

ADD_LIBRARY(H5FDdsm_f90CStub
  ${H5FDdsm_F90_C_STUB_SRCS}
)

TARGET_LINK_LIBRARIES(H5FDdsm_f90CStub
  H5FDdsm
  ${MPI_LIBRARY}
  ${MPI_EXTRA_LIBRARY}
)

SET_LIB_OPTIONS(H5FDdsm_f90CStub)
IF (WIN32 AND MSVC AND BUILD_SHARED_LIBS)
  SET_PROPERTY(TARGET H5FDdsm_f90CStub
    APPEND PROPERTY COMPILE_DEFINITIONS
    hdf5_f90cstub_EXPORTS)
ENDIF (WIN32 AND MSVC AND BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Build the Fortran library
#-----------------------------------------------------------------------------
SET(H5FDdsm_FORTRAN_SRC
  H5FDdsmf90global.f90
  H5FDdsmff.f90
)

IF (H5FD_DSM_BUILD_STEERING)
  SET(H5FDdsm_FORTRAN_SRC
    H5FDdsmSteeringff.f90
    ${H5FDdsm_FORTRAN_SRC}
  )
ENDIF (H5FD_DSM_BUILD_STEERING)

ADD_LIBRARY(H5FDdsm_fortran
  ${H5FDdsm_FORTRAN_SRC}
)

TARGET_LINK_LIBRARIES(H5FDdsm_fortran
  hdf5_fortran
  H5FDdsm_f90CStub
  hdf5_f90cstub
  ${MPI_Fortran_LIBRARIES}
)

SET_LIB_OPTIONS(H5FDdsm_fortran)

IF (WIN32 AND MSVC)
  SET_TARGET_PROPERTIES (H5FDdsm_fortran
    PROPERTIES
      LINKER_LANGUAGE Fortran
	  LINK_FLAGS "/SUBSYSTEM:CONSOLE"
	)
  IF (BUILD_SHARED_LIBS)
    SET_PROPERTY(TARGET H5FDdsm_fortran
      APPEND PROPERTY COMPILE_DEFINITIONS
      BUILD_H5FD_DSM_DLL)
  ENDIF (BUILD_SHARED_LIBS)
  SET_PROPERTY(TARGET H5FDdsm_fortran
    APPEND PROPERTY COMPILE_DEFINITIONS
    H5FD_DSM_F90_WIN32)
ENDIF (WIN32 AND MSVC)

SET_TARGET_PROPERTIES (H5FDdsm_fortran
  PROPERTIES
    LINKER_LANGUAGE Fortran
)

#-----------------------------------------------------------------------------
# Build the Fortran Tests
#-----------------------------------------------------------------------------
IF(BUILD_TESTING)
  SET(H5FDdsm_FORTRAN_TEST_SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/Testing/dsmexample.f90
  )

  ADD_EXECUTABLE(H5FDdsmFortranTest
    ${H5FDdsm_FORTRAN_TEST_SRC}
  )

  SET_FORTRAN_LINK_OPTIONS(H5FDdsmFortranTest)

  TARGET_LINK_LIBRARIES(H5FDdsmFortranTest
    H5FDdsm_fortran
 )

ENDIF(BUILD_TESTING)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
INSTALL (
  TARGETS
    H5FDdsm_fortran
    H5FDdsm_f90CStub
  EXPORT
    ${H5FD_DSM_EXPORTED_TARGETS}
  LIBRARY DESTINATION ${H5FD_DSM_INSTALL_LIB_DIR} COMPONENT fortlibraries
  ARCHIVE DESTINATION ${H5FD_DSM_INSTALL_LIB_DIR} COMPONENT fortlibraries
  RUNTIME DESTINATION ${H5FD_DSM_INSTALL_BIN_DIR} COMPONENT fortlibraries
)

#-----------------------------------------------------------------------------
# Specify project header files to be installed
#-----------------------------------------------------------------------------
SET(H5FDdsmFortranHeaders
  ${CMAKE_CURRENT_SOURCE_DIR}/H5f90dsmproto.h
)

IF (H5FD_DSM_BUILD_STEERING)
  SET(H5FDdsmFortranHeaders
    ${CMAKE_CURRENT_SOURCE_DIR}/H5f90dsmSteeringproto.h
    ${H5FDdsmFortranHeaders}
  )
ENDIF (H5FD_DSM_BUILD_STEERING)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
INSTALL (
  FILES
    ${H5FDdsmFortranHeaders}
  DESTINATION
    ${H5FD_DSM_INSTALL_INCLUDE_DIR}/fortran
  COMPONENT
    fortheaders
)

INSTALL (
  DIRECTORY
    ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/
  DESTINATION
    ${H5FD_DSM_INSTALL_INCLUDE_DIR}/fortran
  COMPONENT
    fortheaders
)
