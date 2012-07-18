#-----------------------------------------------
# Setup some convenience flags depending on machine
#-----------------------------------------------
if(WIN32)
  set(COMPUTERNAME $ENV{COMPUTERNAME})
  if(COMPUTERNAME)
#    mesage("Setting computername #DEFINE to MACHINE_$ENV{COMPUTERNAME}")
    add_definitions(-DMACHINE_$ENV{COMPUTERNAME})
    if("${COMPUTERNAME}" MATCHES "AGNO")
      add_definitions(-DH5FD_DEBUG_WITH_THREADS)
    endif("${COMPUTERNAME}" MATCHES "AGNO")
  endif(COMPUTERNAME)

#  add_definitions(-DH5FD_TEST_WAIT) # For debug
endif(WIN32)

#-----------------------------------------------
# Compile kwsys library and setup TestDriver
#-----------------------------------------------
set(KWSYS_NAMESPACE H5FDdsm_sys)
set(KWSYS_USE_SystemTools 1)
set(KWSYS_USE_Process 1)
set(KWSYS_USE_RegularExpression 1)
add_subdirectory(kwsys)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/kwsys)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/H5FDdsmTestDriverConfig.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/H5FDdsmTestDriverConfig.h @ONLY ESCAPE_QUOTES)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

add_executable(H5FDdsmTestDriver
  ${CMAKE_CURRENT_SOURCE_DIR}/H5FDdsmTestDriver.cxx
)
target_link_libraries(H5FDdsmTestDriver H5FDdsm_sys)

#-----------------------------------------------
# Set up test options and macros
#-----------------------------------------------
option(H5FDdsm_TESTING_DYNAMIC "Run H5FDdsm dynamic tests" ON)
set(H5FDdsm_TESTING_LENGTH "18" CACHE STRING "DSM total length (in MBytes) used for testing")
set(H5FDdsm_TESTING_BLOCK_LENGTH "2097152" CACHE STRING "DSM block length (in Bytes) used for testing")

set(H5FDdsm_dynamic_comm Socket MPI MPI_RMA)
set(H5FDdsm_static_comm MPI MPI_RMA)
if(H5FDdsm_USE_CRAY_DMAPP)
  set(H5FDdsm_static_comm ${H5FDdsm_static_comm} DMAPP)
endif(H5FDdsm_USE_CRAY_DMAPP)

macro(add_h5fd_dsm_parallel_test full_test_name test_prog)
  add_test(NAME "${full_test_name}"
    COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
    ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${test_prog}> ${MPIEXEC_POSTFLAGS}
  )
endmacro(add_h5fd_dsm_parallel_test)

function(add_h5fd_dsm_dynamic_test full_test_name server client)
  foreach(comm ${H5FDdsm_dynamic_comm})
    add_test("${full_test_name}_${comm}_dynamic"
      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/H5FDdsmTestDriver
      --server ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${server}
      --client ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${client}
      ${H5FDdsm_TESTING_LENGTH} ${comm}
    )
  endforeach(comm)
endfunction(add_h5fd_dsm_dynamic_test)

macro(add_h5fd_dsm_static_test full_test_name server client)
  foreach(comm ${H5FDdsm_static_comm})
    add_test(NAME "${full_test_name}_${comm}_static"
      COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
      ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${server}>
      ${H5FDdsm_TESTING_LENGTH} ${comm} "Static" : ${MPIEXEC_NUMPROC_FLAG}
      ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${client}> 0 ${comm} "Static"
    )
  endforeach(comm)
endmacro(add_h5fd_dsm_static_test)

function(add_h5fd_dsm_dynamic_block_test full_test_name blocktype server client)
  foreach(comm ${H5FDdsm_dynamic_comm})
    add_test("${full_test_name}_${comm}_dynamic_${blocktype}"
      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/H5FDdsmTestDriver
      --server ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${server}
      --client ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${client}
      ${H5FDdsm_TESTING_LENGTH} ${comm} "Dynamic" ${blocktype}
      ${H5FDdsm_TESTING_BLOCK_LENGTH}
    )
  endforeach(comm)
endfunction(add_h5fd_dsm_dynamic_block_test)

macro(add_h5fd_dsm_static_block_test full_test_name blocktype server client)
  foreach(comm ${H5FDdsm_static_comm})
    add_test(NAME "${full_test_name}_${comm}_static_${blocktype}"
      COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
      ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${server}>
      ${H5FDdsm_TESTING_LENGTH} ${comm} "Static" ${blocktype}
      ${H5FDdsm_TESTING_BLOCK_LENGTH} : ${MPIEXEC_NUMPROC_FLAG}
      ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${client}> 0 ${comm} "Static"
    )
  endforeach(comm)
endmacro(add_h5fd_dsm_static_block_test)

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
add_library(H5FDdsmTesting STATIC
  H5FDdsmTest.cxx
)
target_link_libraries(H5FDdsmTesting
  H5FDdsmTools
  H5FDdsm_sys
)
set_lib_options(H5FDdsmTesting "H5FDdsmTesting" STATIC)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmTesting)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmStandAlone : Simple parallel write/read test to a DSM (not connected)
#-----------------------------------------------------------------------------
add_executable(H5FDdsmStandAlone
  H5FDdsmStandAlone.cxx
)
target_link_libraries(H5FDdsmStandAlone
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmStandAlone)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmAbortPublish : Simple parallel write/read test to abort DSM publish
# state when no connection has been established yet
#-----------------------------------------------------------------------------
add_executable(H5FDdsmAbortPublish
  H5FDdsmAbortPublish.cxx
)
target_link_libraries(H5FDdsmAbortPublish
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmAbortPublish)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_auto : Automatically create a DSM object and send data
#-----------------------------------------------------------------------------
add_executable(H5FDdsmSender_cwrite_auto
  H5FDdsmSender_cwrite_auto.c
)
target_link_libraries(H5FDdsmSender_cwrite_auto
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmSender_cwrite_auto)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_auto : Allocate DSM and dump reveived data
#-----------------------------------------------------------------------------
add_executable(H5FDdsmReceiver_cwrite_auto
  H5FDdsmReceiver_cwrite_auto.cxx
)
target_link_libraries(H5FDdsmReceiver_cwrite_auto
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmReceiver_cwrite_auto)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_auto_serial : Automatically create a DSM object and
# send data using DSM in serial mode
#-----------------------------------------------------------------------------
add_executable(H5FDdsmSender_cwrite_auto_serial
  H5FDdsmSender_cwrite_auto_serial.c
)
target_link_libraries(H5FDdsmSender_cwrite_auto_serial
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmSender_cwrite_auto_serial)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_auto_serial : Allocate DSM and dump reveived data
#-----------------------------------------------------------------------------
add_executable(H5FDdsmReceiver_cwrite_auto_serial
  H5FDdsmReceiver_cwrite_auto_serial.cxx
)
target_link_libraries(H5FDdsmReceiver_cwrite_auto_serial
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmReceiver_cwrite_auto_serial)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_nohdf_bw : Bandwidth test of DSM write operations
#-----------------------------------------------------------------------------
#add_executable(H5FDdsmSender_cwrite_nohdf_bw
#  H5FDdsmSender_cwrite_nohdf_bw.cxx
#)
#target_link_libraries(H5FDdsmSender_cwrite_nohdf_bw
#  H5FDdsmTesting
#)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_bw : Bandwidth test of DSM write operations
#-----------------------------------------------------------------------------
add_executable(H5FDdsmSender_cwrite_bw
  H5FDdsmSender_cwrite_bw.cxx
)
target_link_libraries(H5FDdsmSender_cwrite_bw
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmSender_cwrite_bw)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_bw : Bandwidth test receiver (does not do anything except hosting data)
#-----------------------------------------------------------------------------
add_executable(H5FDdsmReceiver_cwrite_bw
  H5FDdsmReceiver_cwrite_bw.cxx
)
target_link_libraries(H5FDdsmReceiver_cwrite_bw
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmReceiver_cwrite_bw)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_sread_bw : Data validity test of DSM write operations
#-----------------------------------------------------------------------------
add_executable(H5FDdsmSender_cwrite_sread_bw
  H5FDdsmSender_cwrite_sread_bw.cxx
)
target_link_libraries(H5FDdsmSender_cwrite_sread_bw
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmSender_cwrite_sread_bw)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_sread_bw : Data validity test receiver - Checks received data
#-----------------------------------------------------------------------------
add_executable(H5FDdsmReceiver_cwrite_sread_bw
  H5FDdsmReceiver_cwrite_sread_bw.cxx
)
target_link_libraries(H5FDdsmReceiver_cwrite_sread_bw
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmReceiver_cwrite_sread_bw)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_cread : Send data as a client and test RDONLY mode
#-----------------------------------------------------------------------------
add_executable(H5FDdsmSender_cwrite_cread
  H5FDdsmSender_cwrite_cread.cxx
)
target_link_libraries(H5FDdsmSender_cwrite_cread
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmSender_cwrite_cread)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_cread : Receive data as a server and dump the data
#-----------------------------------------------------------------------------
add_executable(H5FDdsmReceiver_cwrite_cread
  H5FDdsmReceiver_cwrite_cread.cxx
)
target_link_libraries(H5FDdsmReceiver_cwrite_cread
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmReceiver_cwrite_cread)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_sread_cwrite : Send data as a client and test RDWR mode
#-----------------------------------------------------------------------------
add_executable(H5FDdsmSender_cwrite_sread_cwrite
  H5FDdsmSender_cwrite_sread_cwrite.cxx
)
target_link_libraries(H5FDdsmSender_cwrite_sread_cwrite
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmSender_cwrite_sread_cwrite)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_sread_cwrite : Receive data as a server and dump the data
#-----------------------------------------------------------------------------
add_executable(H5FDdsmReceiver_cwrite_sread_cwrite
  H5FDdsmReceiver_cwrite_sread_cwrite.cxx
)
target_link_libraries(H5FDdsmReceiver_cwrite_sread_cwrite
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmReceiver_cwrite_sread_cwrite)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmSender_cwrite_swrite_cread : Send data as a client, wait for the server to give
#                      hand back and dump the data
#-----------------------------------------------------------------------------
add_executable(H5FDdsmSender_cwrite_swrite_cread
  H5FDdsmSender_cwrite_swrite_cread.cxx
)
target_link_libraries(H5FDdsmSender_cwrite_swrite_cread
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmSender_cwrite_swrite_cread)
endif(H5FDdsm_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# H5FDdsmReceiver_cwrite_swrite_cread : Receive data as a server, dump the data, write back
#                        an array into a newly created steering group
#-----------------------------------------------------------------------------
add_executable(H5FDdsmReceiver_cwrite_swrite_cread
  H5FDdsmReceiver_cwrite_swrite_cread.cxx
)
target_link_libraries(H5FDdsmReceiver_cwrite_swrite_cread
  H5FDdsmTesting
)
if(H5FDdsm_ENABLE_COVERAGE)
  set_coverage_flags(H5FDdsmReceiver_cwrite_swrite_cread)
endif(H5FDdsm_ENABLE_COVERAGE)

if(H5FDdsm_BUILD_STEERING)
  #-----------------------------------------------------------------------------
  # H5FDdsmSender_steer : Query steering update, Send data, wait for the server
  #                      to give hand back and dump the data
  #-----------------------------------------------------------------------------
  add_executable(H5FDdsmSender_steer
    H5FDdsmSender_steer.cxx
  )
  target_link_libraries(H5FDdsmSender_steer
    H5FDdsmTesting
  )
  if(H5FDdsm_ENABLE_COVERAGE)
    set_coverage_flags(H5FDdsmSender_steer)
  endif(H5FDdsm_ENABLE_COVERAGE)

  #-----------------------------------------------------------------------------
  # H5FDdsmReceiver_steer : Receive data as a server, dump the data, 
  #                        update steered objects
  #-----------------------------------------------------------------------------
  add_executable(H5FDdsmReceiver_steer
    H5FDdsmReceiver_steer.cxx
  )
  target_link_libraries(H5FDdsmReceiver_steer
    H5FDdsmTesting
  )
  if(H5FDdsm_ENABLE_COVERAGE)
    set_coverage_flags(H5FDdsmReceiver_steer)
  endif(H5FDdsm_ENABLE_COVERAGE)

  #-----------------------------------------------------------------------------
  # H5FDdsmSender_async1 : tests involving repeat async send receive
  #-----------------------------------------------------------------------------
  add_executable(H5FDdsmSender_async1
    H5FDdsmSender_async1.cxx
  )
  target_link_libraries(H5FDdsmSender_async1
    H5FDdsmTesting
  )
  if(H5FDdsm_ENABLE_COVERAGE)
    set_coverage_flags(H5FDdsmSender_async1)
  endif(H5FDdsm_ENABLE_COVERAGE)

  #-----------------------------------------------------------------------------
  # H5FDdsmReceiver_async1 : test involving repeat send receive
  #-----------------------------------------------------------------------------
  add_executable(H5FDdsmReceiver_async1
    H5FDdsmReceiver_async1.cxx
  )
  target_link_libraries(H5FDdsmReceiver_async1
    H5FDdsmTesting
  )
  if(H5FDdsm_ENABLE_COVERAGE)
    set_coverage_flags(H5FDdsmReceiver_async1)
  endif(H5FDdsm_ENABLE_COVERAGE)

  #-----------------------------------------------------------------------------
endif(H5FDdsm_BUILD_STEERING)

if(MPIEXEC)
  set(H5FDdsm_tests
    cwrite_auto
    cwrite_auto_serial
    cwrite_bw
    cwrite_sread_bw
    cwrite_cread
    cwrite_sread_cwrite
    cwrite_swrite_cread
    async1
  )
  if(H5FDdsm_BUILD_STEERING)
    set(H5FDdsm_tests ${H5FDdsm_tests} steer)
  endif(H5FDdsm_BUILD_STEERING)
  add_h5fd_dsm_parallel_test(H5FDdsm_StandAlone H5FDdsmStandAlone)
  add_h5fd_dsm_parallel_test(H5FDdsm_AbortPublish H5FDdsmAbortPublish)
  foreach(H5FDdsm_test ${H5FDdsm_tests})
    if(H5FDdsm_TESTING_DYNAMIC)
      add_h5fd_dsm_dynamic_test(H5FDdsm_${H5FDdsm_test}
        H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
      add_h5fd_dsm_dynamic_block_test(H5FDdsm_${H5FDdsm_test}
        Block  H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
      add_h5fd_dsm_dynamic_block_test(H5FDdsm_${H5FDdsm_test}
        RBlock H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
    endif(H5FDdsm_TESTING_DYNAMIC)
    add_h5fd_dsm_static_test(H5FDdsm_${H5FDdsm_test}
      H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
    add_h5fd_dsm_static_block_test(H5FDdsm_${H5FDdsm_test}
      Block  H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
    add_h5fd_dsm_static_block_test(H5FDdsm_${H5FDdsm_test}
      RBlock H5FDdsmReceiver_${H5FDdsm_test} H5FDdsmSender_${H5FDdsm_test})
  endforeach(H5FDdsm_test)
endif(MPIEXEC)
