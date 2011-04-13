#-----------------------------------------------
# Setup some convenience flags depending on machine
#-----------------------------------------------
IF (WIN32)
  SET(COMPUTERNAME $ENV{COMPUTERNAME})
  IF(COMPUTERNAME)
    MESSAGE("Setting computername #DEFINE to MACHINE_$ENV{COMPUTERNAME}")
    ADD_DEFINITIONS(-DMACHINE_$ENV{COMPUTERNAME})
    IF ("${COMPUTERNAME}" MATCHES "AGNO")
      ADD_DEFINITIONS(-DH5FD_DEBUG_WITH_THREADS)
    ENDIF ("${COMPUTERNAME}" MATCHES "AGNO")
  ENDIF(COMPUTERNAME)

#  ADD_DEFINITIONS(-DH5FD_TEST_WAIT) # For debug
ENDIF(WIN32)

#-----------------------------------------------
# Compile kwsys library and setup TestDriver
#-----------------------------------------------
SET(KWSYS_NAMESPACE H5FDdsm_sys)
SET(KWSYS_USE_SystemTools 1)
SET(KWSYS_USE_Process 1)
SET(KWSYS_USE_RegularExpression 1)
ADD_SUBDIRECTORY(kwsys)
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR}/kwsys)

CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/H5FDdsmTestDriverConfig.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/H5FDdsmTestDriverConfig.h @ONLY ESCAPE_QUOTES)
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

ADD_EXECUTABLE(H5FDdsmTestDriver
  ${CMAKE_CURRENT_SOURCE_DIR}/H5FDdsmTestDriver.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmTestDriver H5FDdsm_sys)

SET(COMM_LIST Socket MPI MPI_RMA)
FUNCTION(ADD_REMOTE_DSM_TEST full_test_name server client)
  FOREACH(comm ${COMM_LIST})
    ADD_TEST("${full_test_name}_${comm}"
      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/H5FDdsmTestDriver
      --server ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${server}
      --client ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${client}
      64 ${comm}
      )
  ENDFOREACH(comm)
ENDFUNCTION (ADD_REMOTE_DSM_TEST)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest1 : Bandwidth test of DSM write operations
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest1
  H5FDdsmSenderTest1.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest1
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSenderTest1)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest1 : Bandwidth test receiver (does not do anything except hosting data)
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest1
  H5FDdsmReceiverTest1.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest1
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiverTest1)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest2 : Data validity test of DSM write operations
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest2
  H5FDdsmSenderTest2.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest2
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSenderTest2)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest2 : Data validity test receiver - Checks received data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest2
  H5FDdsmReceiverTest2.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest2
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiverTest2)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest3 : Send data as a client
#-----------------------------------------------------------------------------
#ADD_EXECUTABLE(H5FDdsmSenderTest3
#  H5FDdsmSenderTest3.cxx
#)
#TARGET_LINK_LIBRARIES(H5FDdsmSenderTest3
#  H5FDdsm
#)
#SET_LINK_OPTIONS(H5FDdsmSenderTest3)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest3 : Receive data as a server
#            (auto-allocated - requires H5FD_DSM_SERVER_CONFIG_PATH to be set)
#-----------------------------------------------------------------------------
#ADD_EXECUTABLE(H5FDdsmReceiverTest3
#  H5FDdsmReceiverTest3.cxx
#)
#TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest3
#  H5FDdsm
#)
#SET_LINK_OPTIONS(H5FDdsmReceiverTest3)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest4 : Send data as a client and test RDONLY mode
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest4
  H5FDdsmSenderTest4.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest4
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSenderTest4)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest4 : Receive data as a server and dump the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest4
  H5FDdsmReceiverTest4.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest4
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiverTest4)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest5 : Send data as a client and test RDWR mode
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest5
  H5FDdsmSenderTest5.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest5
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSenderTest5)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest5 : Receive data as a server and dump the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest5
  H5FDdsmReceiverTest5.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest5
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiverTest5)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest6 : Send data as a client, wait for the server to give
#                      hand back and dump the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest6
  H5FDdsmSenderTest6.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest6
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSenderTest6)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest6 : Receive data as a server, dump the data, write back
#                        an array into a newly created steering group
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest6
  H5FDdsmReceiverTest6.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest6
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiverTest6)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest7 : Send data as a client, wait for the server to give
#                      hand back and dump the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest7
  H5FDdsmSenderTest7.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest7
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSenderTest7)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest7 : Receive data as a server, dump the data, write back
#                        an array into a newly created steering group
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest7
  H5FDdsmReceiverTest7.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest7
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiverTest7)

#-----------------------------------------------------------------------------
IF (MPIEXEC)
  ADD_REMOTE_DSM_TEST(H5FDdsmTest1 H5FDdsmReceiverTest1 H5FDdsmSenderTest1)
  ADD_REMOTE_DSM_TEST(H5FDdsmTest2 H5FDdsmReceiverTest2 H5FDdsmSenderTest2)
  ADD_REMOTE_DSM_TEST(H5FDdsmTest4 H5FDdsmReceiverTest4 H5FDdsmSenderTest4)
  ADD_REMOTE_DSM_TEST(H5FDdsmTest5 H5FDdsmReceiverTest5 H5FDdsmSenderTest5)
  ADD_REMOTE_DSM_TEST(H5FDdsmTest6 H5FDdsmReceiverTest6 H5FDdsmSenderTest6)
  ADD_REMOTE_DSM_TEST(H5FDdsmTest7 H5FDdsmReceiverTest7 H5FDdsmSenderTest7)
ENDIF (MPIEXEC)
