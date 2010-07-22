IF (WIN32)
  ADD_DEFINITIONS(-DH5FD_TEST_WAIT)
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
