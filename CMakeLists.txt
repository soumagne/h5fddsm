cmake_minimum_required (VERSION 2.8)
PROJECT (H5FDdsm C CXX)

#-----------------------------------------------------------------------------
# Choose static or shared libraries.
#-----------------------------------------------------------------------------
OPTION(BUILD_SHARED_LIBS "Build with shared libraries." OFF)

# Set unique var for using in config file
IF(BUILD_SHARED_LIBS)
  SET(H5FDdsm_BUILD_SHARED_LIBS 1)
ELSE(BUILD_SHARED_LIBS)
  SET(H5FDdsm_BUILD_SHARED_LIBS 0)
ENDIF(BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Find HDF5
#-----------------------------------------------------------------------------
INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/CMake/FindHDF5.cmake)
IF(NOT HDF5_FOUND)
  MESSAGE(FATAL_ERROR "This project requires HDF5")
ENDIF(NOT HDF5_FOUND)
  
INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIR})

#-----------------------------------------------------------------------------
# Find MPI
#-----------------------------------------------------------------------------
FIND_PACKAGE(MPI)

INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})

#-----------------------------------------------------------------------------
# Configure our module header file
#-----------------------------------------------------------------------------
CONFIGURE_FILE(
  ${H5FDdsm_SOURCE_DIR}/H5FDdsmConfig.h.in 
  ${H5FDdsm_BINARY_DIR}/H5FDdsmConfig.h
)

INCLUDE_DIRECTORIES(${H5FDdsm_BINARY_DIR})

#-----------------------------------------------------------------------------
# Define a user config path for the dsm ini file
#-----------------------------------------------------------------------------
SET(H5FDdsm_CONFIG_PATH "" CACHE PATH "Place to store stuff")

#-----------------------------------------------------------------------------
# Specify project source files
#-----------------------------------------------------------------------------
SET(H5FDdsmSources
  H5FDdsm.cxx
  H5FDdsmBuffer.cxx
  H5FDdsmComm.cxx
  H5FDdsmCommMpi.cxx
  H5FDdsmCommSocket.cxx
  H5FDdsmDriver.cxx
  H5FDdsmIniFile.cxx
  H5FDdsmObject.cxx
  H5FDdsmMsg.cxx
  H5FDdsmSocket.cxx
  H5FDdsmStorage.cxx
)

#-----------------------------------------------------------------------------
# Create the library
#-----------------------------------------------------------------------------
ADD_LIBRARY(H5FDdsm ${H5FDdsmSources})

TARGET_LINK_LIBRARIES(H5FDdsm
  ${HDF5_LIBRARY} 
  ${MPI_LIBRARY}
) 

IF(WIN32)
  TARGET_LINK_LIBRARIES(H5FDdsm
    wsock32
  )  
  ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS)
ENDIF(WIN32)

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
OPTION(BUILD_TESTING "Build Testing code" OFF)

IF(BUILD_TESTING)
 ADD_SUBDIRECTORY(Testing)
ENDIF(BUILD_TESTING)
 
#-----------------------------------------------------------------------------
# Debugging below here
#-----------------------------------------------------------------------------
MESSAGE("MPI_LIBRARY is ${MPI_LIBRARY}, MPI dir is ${MPI_INCLUDE_PATH}, HDF5 dir is ${HDF5_INCLUDE_DIR}")
