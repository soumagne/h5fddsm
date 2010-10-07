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
# H5FDdsmSenderTest : Send data as fast as possible across the network
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest 
  H5FDdsmSenderTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest 
  H5FDdsm
)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest : Receive data from Sender and do a light H5Dump
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest 
  H5FDdsmReceiverTest.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest
  H5FDdsm
)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest2 : Send data as fast as possible across the network
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest2 
  H5FDdsmSenderTest2.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest2 
  H5FDdsm
)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest2 : Receive data from Sender and do a light H5Dump
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest2 
  H5FDdsmReceiverTest2.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest2
  H5FDdsm
)

#-----------------------------------------------------------------------------
# H5FDdsmSenderTest3 : Send data as a client
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmSenderTest3 
  H5FDdsmSenderTest3.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmSenderTest3 
  H5FDdsm
)

#-----------------------------------------------------------------------------
# H5FDdsmReceiverTest3 : Receive data as a server and check the data
#-----------------------------------------------------------------------------
ADD_EXECUTABLE(H5FDdsmReceiverTest3 
  H5FDdsmReceiverTest3.cxx
)
TARGET_LINK_LIBRARIES(H5FDdsmReceiverTest3
  H5FDdsm
)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install 
#-----------------------------------------------------------------------------
INSTALL (
  TARGETS
    H5FDdsmSenderTest
    H5FDdsmReceiverTest
  EXPORT
    ${H5FD_DSM_EXPORTED_TARGETS}
  LIBRARY DESTINATION lib 
  ARCHIVE DESTINATION lib 
  RUNTIME DESTINATION bin 
)
