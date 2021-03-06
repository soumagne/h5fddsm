cmake_minimum_required(VERSION 2.8)
project(H5FDdsm_F90 C CXX Fortran)

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

if(H5FDdsm_BUILD_STEERING)
  set(H5FDdsm_F90_C_STUB_SRCS
    H5FDdsmSteeringf.c
    ${H5FDdsm_F90_C_STUB_SRCS}
  )
endif(H5FDdsm_BUILD_STEERING)

if(H5FDdsm_BUILD_TOOLS)
  set(H5FDdsm_F90_C_STUB_SRCS
    H5FDdsmToolsf.c
    ${H5FDdsm_F90_C_STUB_SRCS}
  )
  set(H5FDdsm_f90CStub_LIB_DEPENDENCIES H5FDdsmTools)
else(H5FDdsm_BUILD_TOOLS)
  set(H5FDdsm_f90CStub_LIB_DEPENDENCIES H5FDdsm)
endif(H5FDdsm_BUILD_TOOLS)

add_library(H5FDdsm_f90CStub
  ${H5FDdsm_F90_C_STUB_SRCS}
)

target_link_libraries(H5FDdsm_f90CStub
  ${H5FDdsm_f90CStub_LIB_DEPENDENCIES}
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

if(H5FDdsm_BUILD_STEERING)
  set(H5FDdsm_FORTRAN_SRC
    H5FDdsmSteeringff.f90
    ${H5FDdsm_FORTRAN_SRC}
  )
endif(H5FDdsm_BUILD_STEERING)

if(H5FDdsm_BUILD_TOOLS)
  set(H5FDdsm_FORTRAN_SRC
    H5FDdsmToolsff.f90
    ${H5FDdsm_FORTRAN_SRC}
  )
endif(H5FDdsm_BUILD_TOOLS)

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
      BUILD_H5FDdsm_DLL)
  endif(BUILD_SHARED_LIBS)
  set_property(TARGET H5FDdsm_fortran
    APPEND PROPERTY COMPILE_DEFINITIONS
    H5FDdsm_F90_WIN32)
endif(WIN32 AND MSVC)

set_target_properties(H5FDdsm_fortran
  PROPERTIES
    LINKER_LANGUAGE Fortran
)

#-----------------------------------------------------------------------------
# Configure our module header files
#-----------------------------------------------------------------------------
configure_file(
  ${H5FDdsm_F90_SOURCE_DIR}/H5FDdsmF90Config.h.in 
  ${H5FDdsm_F90_BINARY_DIR}/H5FDdsmF90Config.h
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
    ${H5FDdsm_EXPORTED_TARGETS}
  LIBRARY DESTINATION ${H5FDdsm_INSTALL_LIB_DIR} COMPONENT fortlibraries
  ARCHIVE DESTINATION ${H5FDdsm_INSTALL_LIB_DIR} COMPONENT fortlibraries
  RUNTIME DESTINATION ${H5FDdsm_INSTALL_BIN_DIR} COMPONENT fortlibraries
)

#-----------------------------------------------------------------------------
# Specify project header files to be installed
#-----------------------------------------------------------------------------
set(H5FDdsmFortranHeaders
  ${CMAKE_CURRENT_SOURCE_DIR}/H5f90dsmproto.h
)

if(H5FDdsm_BUILD_STEERING)
  set(H5FDdsmFortranHeaders
    ${CMAKE_CURRENT_SOURCE_DIR}/H5f90dsmSteeringproto.h
    ${H5FDdsm_F90_BINARY_DIR}/H5FDdsmF90Config.h
    ${H5FDdsmFortranHeaders}
  )
endif(H5FDdsm_BUILD_STEERING)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
install(
  FILES
    ${H5FDdsmFortranHeaders}
  DESTINATION
    ${H5FDdsm_INSTALL_INCLUDE_DIR}/fortran
  COMPONENT
    fortheaders
)

install(
  DIRECTORY
    ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/
  DESTINATION
    ${H5FDdsm_INSTALL_INCLUDE_DIR}/fortran
  COMPONENT
    fortheaders
)
