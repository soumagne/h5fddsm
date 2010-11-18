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

  ADD_DEFINITIONS(-DH5FD_TEST_WAIT) # For debug
ENDIF(WIN32)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest : Bandwidth test of DSM write operations
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest
  H5FDdsmSenderTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSenderTest)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest : Bandwidth test receiver (does not do anything except hosting data)
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest
  H5FDdsmReceiverTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiverTest)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest2 : Data validity test of DSM write operations
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest2
  H5FDdsmSenderTest2.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest2
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSenderTest2)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest2 : Data validity test receiver - Checks received data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest2
  H5FDdsmReceiverTest2.cxx
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
  H5FDdsmSenderTest4.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest4
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSenderTest4)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest4 : Receive data as a server and dumps the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest4
  H5FDdsmReceiverTest4.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest4
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiverTest4)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest5 : Send data as a client and test RDWR mode
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest5
  H5FDdsmSenderTest5.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest5
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmSenderTest5)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest5 : Receive data as a server and dumps the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest5
  H5FDdsmReceiverTest5.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest5
  H5FDdsm
)
SET_LINK_OPTIONS(H5FDdsmReceiverTest5)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install
#-----------------------------------------------------------------------------
INSTALL (
  TARGETS
    H5FDdsmSenderTest
    H5FDdsmReceiverTest
    H5FDdsmSenderTest2
    H5FDdsmReceiverTest2
    H5FDdsmSenderTest4
    H5FDdsmReceiverTest4
    H5FDdsmSenderTest5
    H5FDdsmReceiverTest5
  EXPORT
    ${H5FD_DSM_EXPORTED_TARGETS}
  LIBRARY DESTINATION lib${LIB_SUFFIX}
  ARCHIVE DESTINATION lib${LIB_SUFFIX}
  RUNTIME DESTINATION bin
)
