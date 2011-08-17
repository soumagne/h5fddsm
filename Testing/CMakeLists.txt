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

#-----------------------------------------------
# Set up test options and macros
#-----------------------------------------------
OPTION(H5FD_DSM_TESTING_DYNAMIC "Run H5FDdsm dynamic tests" ON)
SET(H5FD_DSM_TESTING_LENGTH "18" CACHE STRING "DSM total length (in MBytes) used for testing")
SET(H5FD_DSM_TESTING_BLOCK_LENGTH "2097152" CACHE STRING "DSM block length (in Bytes) used for testing")

MACRO(ADD_H5FD_DSM_PARALLEL_TEST full_test_name test_prog)
  ADD_TEST(NAME "${full_test_name}"
    COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
    ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${test_prog}> ${MPIEXEC_POSTFLAGS}
  )
ENDMACRO(ADD_H5FD_DSM_PARALLEL_TEST)

FUNCTION(ADD_H5FD_DSM_DYNAMIC_TEST full_test_name server client)
  FOREACH(comm Socket MPI MPI_RMA)
    ADD_TEST("${full_test_name}_${comm}_dynamic"
      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/H5FDdsmTestDriver
      --server ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${server}
      --client ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${client}
      ${H5FD_DSM_TESTING_LENGTH} ${comm}
    )
  ENDFOREACH(comm)
ENDFUNCTION(ADD_H5FD_DSM_DYNAMIC_TEST)

MACRO(ADD_H5FD_DSM_STATIC_TEST full_test_name server client)
  FOREACH(comm MPI MPI_RMA)
    ADD_TEST(NAME "${full_test_name}_${comm}_static"
      COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
      ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${server}>
      ${H5FD_DSM_TESTING_LENGTH} ${comm} "Static" : ${MPIEXEC_NUMPROC_FLAG}
      ${MPIEXEC_MAX_NUMPROCS} $<TARGET_FILE:${client}> 0 ${comm}
    )
  ENDFOREACH(comm)
ENDMACRO(ADD_H5FD_DSM_STATIC_TEST)

FUNCTION(ADD_H5FD_DSM_DYNAMIC_BLOCK_TEST full_test_name blocktype server client)
  FOREACH(comm Socket MPI MPI_RMA)
    ADD_TEST("${full_test_name}_${comm}_dynamic_${blocktype}"
      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/H5FDdsmTestDriver
      --server ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${server}
      --client ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${client}
      ${H5FD_DSM_TESTING_LENGTH} ${comm} "Dynamic" ${blocktype}
      ${H5FD_DSM_TESTING_BLOCK_LENGTH}
    )
  ENDFOREACH(comm)
ENDFUNCTION(ADD_H5FD_DSM_DYNAMIC_BLOCK_TEST)

MACRO(ADD_H5FD_DSM_STATIC_BLOCK_TEST full_test_name blocktype server client)
  FOREACH(comm MPI MPI_RMA)
    ADD_TEST(NAME "${full_test_name}_${comm}_static_${blocktype}"
      COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
      ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${server}>
      ${H5FD_DSM_TESTING_LENGTH} ${comm} "Static" ${blocktype}
      ${H5FD_DSM_TESTING_BLOCK_LENGTH} : ${MPIEXEC_NUMPROC_FLAG}
      ${MPIEXEC_MAX_NUMPROCS} $<TARGET_FILE:${client}> 0 ${comm}
    )
  ENDFOREACH(comm)
ENDMACRO(ADD_H5FD_DSM_STATIC_BLOCK_TEST)

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
# H5FDdsmTest : Lib used by tests contains main test initialization etc
#-----------------------------------------------------------------------------
ADD_LIBRARY(H5FDdsmTesting STATIC
  H5FDdsmTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmTesting
  H5FDdsm
)
SET_LIB_OPTIONS(H5FDdsmTesting "H5FDdsmTesting" STATIC)

#-----------------------------------------------------------------------------
# H5FDdsmStandAlone : Simple parallel write/read test to a DSM (not connected)
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmStandAlone
  H5FDdsmStandAlone.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmStandAlone
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmAbortPublish : Simple parallel write/read test to abort DSM publish
# state when no connection has been established yet
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmAbortPublish
  H5FDdsmAbortPublish.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmAbortPublish
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_nohdf_bw : Bandwidth test of DSM write operations
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSender_cwrite_nohdf_bw
  H5FDdsmSender_cwrite_nohdf_bw.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSender_cwrite_nohdf_bw
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_bw : Bandwidth test of DSM write operations
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSender_cwrite_bw
  H5FDdsmSender_cwrite_bw.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSender_cwrite_bw
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_bw : Bandwidth test receiver (does not do anything except hosting data)
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiver_cwrite_bw
  H5FDdsmReceiver_cwrite_bw.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiver_cwrite_bw
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_sread_bw : Data validity test of DSM write operations
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSender_cwrite_sread_bw
  H5FDdsmSender_cwrite_sread_bw.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSender_cwrite_sread_bw
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_sread_bw : Data validity test receiver - Checks received data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiver_cwrite_sread_bw
  H5FDdsmReceiver_cwrite_sread_bw.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiver_cwrite_sread_bw
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_cread : Send data as a client and test RDONLY mode
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSender_cwrite_cread
  H5FDdsmSender_cwrite_cread.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSender_cwrite_cread
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_cread : Receive data as a server and dump the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiver_cwrite_cread
  H5FDdsmReceiver_cwrite_cread.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiver_cwrite_cread
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_sread_cwrite : Send data as a client and test RDWR mode
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSender_cwrite_sread_cwrite
  H5FDdsmSender_cwrite_sread_cwrite.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSender_cwrite_sread_cwrite
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_sread_cwrite : Receive data as a server and dump the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiver_cwrite_sread_cwrite
  H5FDdsmReceiver_cwrite_sread_cwrite.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiver_cwrite_sread_cwrite
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_swrite_cread : Send data as a client, wait for the server to give
#                      hand back and dump the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSender_cwrite_swrite_cread
  H5FDdsmSender_cwrite_swrite_cread.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSender_cwrite_swrite_cread
  H5FDdsmTesting
)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_swrite_cread : Receive data as a server, dump the data, write back
#                        an array into a newly created steering group
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiver_cwrite_swrite_cread
  H5FDdsmReceiver_cwrite_swrite_cread.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiver_cwrite_swrite_cread
  H5FDdsmTesting
)

IF(H5FD_DSM_BUILD_STEERING)
  #-----------------------------------------------------------------------------
  # H5FDdsmSender_steer : Query steering update, Send data, wait for the server
  #                      to give hand back and dump the data
  #-----------------------------------------------------------------------------
  ADD_EXECUTABLE(H5FDdsmSender_steer
    H5FDdsmSender_steer.cxx
  )
  TARGET_LINK_LIBRARIES(H5FDdsmSender_steer
    H5FDdsmTesting
  )

  #-----------------------------------------------------------------------------
  # H5FDdsmReceiver_steer : Receive data as a server, dump the data, 
  #                        update steered objects
  #-----------------------------------------------------------------------------
  ADD_EXECUTABLE(H5FDdsmReceiver_steer
    H5FDdsmReceiver_steer.cxx
  )
  TARGET_LINK_LIBRARIES(H5FDdsmReceiver_steer
    H5FDdsmTesting
  )

  #-----------------------------------------------------------------------------
ENDIF(H5FD_DSM_BUILD_STEERING)

IF (MPIEXEC)
  SET(H5FDdsm_tests
    cwrite_bw
    cwrite_sread_bw
    cwrite_cread
    cwrite_sread_cwrite
    cwrite_swrite_cread
  )
  IF(H5FD_DSM_BUILD_STEERING)
    SET(H5FDdsm_tests ${H5FDdsm_tests} steer)
  ENDIF(H5FD_DSM_BUILD_STEERING)
  ADD_H5FD_DSM_PARALLEL_TEST(H5FDdsm_StandAlone H5FDdsmStandAlone)
  ADD_H5FD_DSM_PARALLEL_TEST(H5FDdsm_AbortPublish H5FDdsmAbortPublish)
  FOREACH(H5FDdsm_test ${H5FDdsm_tests})
    IF (H5FD_DSM_TESTING_DYNAMIC)
      ADD_H5FD_DSM_DYNAMIC_TEST(H5FDdsm_${H5FDdsm_test}
        H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
      ADD_H5FD_DSM_DYNAMIC_BLOCK_TEST(H5FDdsm_${H5FDdsm_test}
        Block  H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
      ADD_H5FD_DSM_DYNAMIC_BLOCK_TEST(H5FDdsm_${H5FDdsm_test}
        RBlock H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
    ENDIF (H5FD_DSM_TESTING_DYNAMIC)
    ADD_H5FD_DSM_STATIC_TEST(H5FDdsm_${H5FDdsm_test}
      H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
    ADD_H5FD_DSM_STATIC_BLOCK_TEST(H5FDdsm_${H5FDdsm_test}
      Block  H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
    ADD_H5FD_DSM_STATIC_BLOCK_TEST(H5FDdsm_${H5FDdsm_test}
      RBlock H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
  ENDFOREACH(H5FDdsm_test)
ENDIF (MPIEXEC)
