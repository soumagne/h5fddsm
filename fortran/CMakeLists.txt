cmake_minimum_required(VERSION 2.8)
project(H5FDdsm_F90 C CXX Fortran)

#-----------------------------------------------------------------------------
# Add MPI Fortran library on win32
#-----------------------------------------------------------------------------
if(WIN32 AND CMAKE_MAJOR_VERSION EQUAL 2 AND CMAKE_MINOR_VERSION EQUAL 8
  AND CMAKE_PATCH_VERSION LESS 5)
# Now defined with CMake v2.8.5
  find_library(MPI_Fortran_LIBRARIES NAMES fmpich2 mpichf90
    HINTS $ENV{ProgramFiles}/MPICH2/lib)
endif(WIN32 AND CMAKE_MAJOR_VERSION EQUAL 2 AND CMAKE_MINOR_VERSION EQUAL 8
  AND CMAKE_PATCH_VERSION LESS 5)

#-----------------------------------------------------------------------------
# Add debug information to default flags (intel Fortran) : JB
#-----------------------------------------------------------------------------
if(CMAKE_Fortran_COMPILER MATCHES ifort)
  if(WIN32 AND MSVC)
    set(CMAKE_Fortran_FLAGS_DEBUG "/debug:full /libs:dll /threads /dbglibs" CACHE
      "Default debug compiler flags" STRING FORCE)
    set(CMAKE_Fortran_FLAGS_RELEASE "/libs:dll /threads" CACHE
      "Default debug compiler flags" STRING FORCE)
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "/debug:full /libs:dll /threads /dbglibs" CACHE
      "Default debug compiler flags" STRING FORCE)
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "/DEBUG" CACHE
      "Default debug linker flags" STRING FORCE)
    message("Adding ifort flags")
  endif(WIN32 AND MSVC)
endif(CMAKE_Fortran_COMPILER MATCHES ifort)

#-----------------------------------------------------------------------------
# Set Include directories for headers, generated files, modules
#-----------------------------------------------------------------------------
include_directories(
  ${H5FDdsm_SOURCE_DIR}
  ${CMAKE_Fortran_MODULE_DIRECTORY}
  ${HDF5_INCLUDE_DIR_FORTRAN}
  ${H5FDdsm_F90_BINARY_DIR}
 )

#-----------------------------------------------------------------------------
# Build the C/Fortran bindings library
#-----------------------------------------------------------------------------
set(H5FDdsm_F90_C_STUB_SRCS
  H5FDdsmf.c
)

if(H5FD_DSM_BUILD_STEERING)
  set(H5FDdsm_F90_C_STUB_SRCS
    H5FDdsmSteeringf.c
    ${H5FDdsm_F90_C_STUB_SRCS}
  )
endif(H5FD_DSM_BUILD_STEERING)

add_library(H5FDdsm_f90CStub
  ${H5FDdsm_F90_C_STUB_SRCS}
)

target_link_libraries(H5FDdsm_f90CStub
  H5FDdsm
  ${MPI_LIBRARY}
)

set_lib_options(H5FDdsm_f90CStub "H5FDdsm_f90CStub" ${H5FDdsm_LIBTYPE})

if(WIN32 AND MSVC AND BUILD_SHARED_LIBS)
  set_property(TARGET H5FDdsm_f90CStub
    APPEND PROPERTY COMPILE_DEFINITIONS
    hdf5_f90cstub_EXPORTS)
endif(WIN32 AND MSVC AND BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Build the Fortran library
#-----------------------------------------------------------------------------
set(H5FDdsm_FORTRAN_SRC
  H5FDdsmf90global.f90
  H5FDdsmff.f90
)

if(H5FD_DSM_BUILD_STEERING)
  set(H5FDdsm_FORTRAN_SRC
    H5FDdsmSteeringff.f90
    ${H5FDdsm_FORTRAN_SRC}
  )
endif(H5FD_DSM_BUILD_STEERING)

add_library(H5FDdsm_fortran
  ${H5FDdsm_FORTRAN_SRC}
)

target_link_libraries(H5FDdsm_fortran
  hdf5_fortran
  H5FDdsm_f90CStub
  hdf5_f90cstub
  ${MPI_Fortran_LIBRARIES}
)

set_lib_options(H5FDdsm_fortran "H5FDdsm_fortran" ${H5FDdsm_LIBTYPE})

if(WIN32 AND MSVC)
  set_target_properties(H5FDdsm_fortran
    PROPERTIES
      LINKER_LANGUAGE Fortran
	  LINK_FLAGS "/SUBSYSTEM:CONSOLE"
	)
  if(BUILD_SHARED_LIBS)
    set_property(TARGET H5FDdsm_fortran
      APPEND PROPERTY COMPILE_DEFINITIONS
      BUILD_H5FD_DSM_DLL)
  endif(BUILD_SHARED_LIBS)
  set_property(TARGET H5FDdsm_fortran
    APPEND PROPERTY COMPILE_DEFINITIONS
    H5FD_DSM_F90_WIN32)
endif(WIN32 AND MSVC)

set_target_properties(H5FDdsm_fortran
  PROPERTIES
    LINKER_LANGUAGE Fortran
)

#-----------------------------------------------------------------------------
# Build the Fortran Tests
#-----------------------------------------------------------------------------
if(BUILD_TESTING)
  set(H5FDdsm_FORTRAN_TEST_SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/Testing/dsmexample.f90
  )

  add_executable(H5FDdsmFortranTest
    ${H5FDdsm_FORTRAN_TEST_SRC}
  )

  set_fortran_link_options(H5FDdsmFortranTest)

  target_link_libraries(H5FDdsmFortranTest
    H5FDdsm_fortran
 )
endif(BUILD_TESTING)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
install(
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
set(H5FDdsmFortranHeaders
  ${CMAKE_CURRENT_SOURCE_DIR}/H5f90dsmproto.h
)

if(H5FD_DSM_BUILD_STEERING)
  set(H5FDdsmFortranHeaders
    ${CMAKE_CURRENT_SOURCE_DIR}/H5f90dsmSteeringproto.h
    ${H5FDdsmFortranHeaders}
  )
endif(H5FD_DSM_BUILD_STEERING)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
install(
  FILES
    ${H5FDdsmFortranHeaders}
  DESTINATION
    ${H5FD_DSM_INSTALL_INCLUDE_DIR}/fortran
  COMPONENT
    fortheaders
)

install(
  DIRECTORY
    ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/
  DESTINATION
    ${H5FD_DSM_INSTALL_INCLUDE_DIR}/fortran
  COMPONENT
    fortheaders
)
