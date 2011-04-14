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

MACRO(ADD_STATIC_DSM_TEST full_test_name server client)
  FOREACH(comm MPI MPI_RMA)
    ADD_TEST(NAME "${full_test_name}_${comm}_static"
      COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
      ${MPIEXEC_PREFLAGS} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${server}
      64 ${comm} Static ${MPIEXEC_POSTFLAGS} : ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} 
      ${MPIEXEC_PREFLAGS} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${client} ${comm} ${MPIEXEC_POSTFLAGS}
    )
  ENDFOREACH(comm)
ENDMACRO(ADD_STATIC_DSM_TEST)

#-----------------------------------------------------------------------------
# Test terminology
# In all the following tests, the DSM is hosted by the server
#-----------------------------------------------------------------------------
# cwrite --> client write        swrite --> server write
# cread  --> client read         sread  --> server read
#
#-----------------------------------------------------------------------------
#
#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_bw : Bandwidth test of DSM write operations
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSender_cwrite_bw
  H5FDdsmSender_cwrite_bw.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSender_cwrite_bw
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSender_cwrite_bw)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_bw : Bandwidth test receiver (does not do anything except hosting data)
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiver_cwrite_bw
  H5FDdsmReceiver_cwrite_bw.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiver_cwrite_bw
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiver_cwrite_bw)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_sread_bw : Data validity test of DSM write operations
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSender_cwrite_sread_bw
  H5FDdsmSender_cwrite_sread_bw.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSender_cwrite_sread_bw
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSender_cwrite_sread_bw)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_sread_bw : Data validity test receiver - Checks received data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiver_cwrite_sread_bw
  H5FDdsmReceiver_cwrite_sread_bw.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiver_cwrite_sread_bw
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiver_cwrite_sread_bw)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_cread : Send data as a client and test RDONLY mode
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSender_cwrite_cread
  H5FDdsmSender_cwrite_cread.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSender_cwrite_cread
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSender_cwrite_cread)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_cread : Receive data as a server and dump the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiver_cwrite_cread
  H5FDdsmReceiver_cwrite_cread.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiver_cwrite_cread
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiver_cwrite_cread)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_sread_cwrite : Send data as a client and test RDWR mode
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSender_cwrite_sread_cwrite
  H5FDdsmSender_cwrite_sread_cwrite.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSender_cwrite_sread_cwrite
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSender_cwrite_sread_cwrite)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_sread_cwrite : Receive data as a server and dump the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiver_cwrite_sread_cwrite
  H5FDdsmReceiver_cwrite_sread_cwrite.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiver_cwrite_sread_cwrite
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiver_cwrite_sread_cwrite)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_swrite_cread : Send data as a client, wait for the server to give
#                      hand back and dump the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSender_cwrite_swrite_cread
  H5FDdsmSender_cwrite_swrite_cread.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSender_cwrite_swrite_cread
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSender_cwrite_swrite_cread)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_swrite_cread : Receive data as a server, dump the data, write back
#                        an array into a newly created steering group
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiver_cwrite_swrite_cread
  H5FDdsmReceiver_cwrite_swrite_cread.cxx H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiver_cwrite_swrite_cread
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiver_cwrite_swrite_cread)

IF(H5FD_DSM_BUILD_STEERING)
  #-----------------------------------------------------------------------------
  # H5FDdsmSender_steer : Query steering update, Send data, wait for the server
  #                      to give hand back and dump the data
  #-----------------------------------------------------------------------------
  ADD_EXECUTABLE(H5FDdsmSender_steer
    H5FDdsmSender_steer.cxx H5FDdsmTest.cxx
  )
  TARGET_LINK_LIBRARIES(H5FDdsmSender_steer
    H5FDdsm
  )
  SET_LINK_OPTIONS(H5FDdsmSender_steer)

  #-----------------------------------------------------------------------------
  # H5FDdsmReceiver_steer : Receive data as a server, dump the data, 
  #                        update steered objects
  #-----------------------------------------------------------------------------
  ADD_EXECUTABLE(H5FDdsmReceiver_steer
    H5FDdsmReceiver_steer.cxx H5FDdsmTest.cxx
  )
  TARGET_LINK_LIBRARIES(H5FDdsmReceiver_steer
    H5FDdsm
  )
  SET_LINK_OPTIONS(H5FDdsmReceiver_steer)

  #-----------------------------------------------------------------------------
ENDIF(H5FD_DSM_BUILD_STEERING)

IF (MPIEXEC)
  ADD_REMOTE_DSM_TEST(H5FDdsm_cwrite_bw H5FDdsmReceiver_cwrite_bw H5FDdsmSender_cwrite_bw)
  ADD_REMOTE_DSM_TEST(H5FDdsm_cwrite_sread_bw H5FDdsmReceiver_cwrite_sread_bw H5FDdsmSender_cwrite_sread_bw)
  ADD_REMOTE_DSM_TEST(H5FDdsm_cwrite_cread H5FDdsmReceiver_cwrite_cread H5FDdsmSender_cwrite_cread)
  ADD_REMOTE_DSM_TEST(H5FDdsm_cwrite_sread_cwrite H5FDdsmReceiver_cwrite_sread_cwrite H5FDdsmSender_cwrite_sread_cwrite)
  ADD_REMOTE_DSM_TEST(H5FDdsm_cwrite_swrite_cread H5FDdsmReceiver_cwrite_swrite_cread H5FDdsmSender_cwrite_swrite_cread)
  IF(H5FD_DSM_BUILD_STEERING)
    ADD_REMOTE_DSM_TEST(H5FDdsm_steer H5FDdsmReceiver_steer H5FDdsmSender_steer)
  ENDIF(H5FD_DSM_BUILD_STEERING)

  ADD_STATIC_DSM_TEST(H5FDdsm_cwrite_bw H5FDdsmReceiver_cwrite_bw H5FDdsmSender_cwrite_bw)
  ADD_STATIC_DSM_TEST(H5FDdsm_cwrite_sread_bw H5FDdsmReceiver_cwrite_sread_bw H5FDdsmSender_cwrite_sread_bw)
  ADD_STATIC_DSM_TEST(H5FDdsm_cwrite_cread H5FDdsmReceiver_cwrite_cread H5FDdsmSender_cwrite_cread)
  ADD_STATIC_DSM_TEST(H5FDdsm_cwrite_sread_cwrite H5FDdsmReceiver_cwrite_sread_cwrite H5FDdsmSender_cwrite_sread_cwrite)
  ADD_STATIC_DSM_TEST(H5FDdsm_cwrite_swrite_cread H5FDdsmReceiver_cwrite_swrite_cread H5FDdsmSender_cwrite_swrite_cread)
  IF(H5FD_DSM_BUILD_STEERING)
    ADD_STATIC_DSM_TEST(H5FDdsm_steer H5FDdsmReceiver_steer H5FDdsmSender_steer)
  ENDIF(H5FD_DSM_BUILD_STEERING)
ENDIF (MPIEXEC)
