// DSM features
#include "H5FDdsmManager.h"

// Sys
#include <cstdlib>
#include <sstream>
#include <mpi.h>

//----------------------------------------------------------------------------
int main (int argc, char* argv[])
{
  H5FDdsmInt32 provided, rank, size;
  MPI_Comm comm = MPI_COMM_WORLD;
  H5FDdsmUInt32 dsmSize = 16; // default MB
  H5FDdsmInt32 commType = H5FD_DSM_COMM_SOCKET;
  H5FDdsmInt32 dsmType = H5FD_DSM_TYPE_UNIFORM;
  H5FDdsmUInt64 dsmBlockSize = 1024;
  H5FDdsmBoolean staticInterComm = false;

  //
  // Receiver will spawn a thread to handle incoming data Put/Get requests
  // we must therefore have MPI_THREAD_MULTIPLE
  //
  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  //
  if (rank == 0) {
    if (provided != MPI_THREAD_MULTIPLE) {
      std::cout << "MPI_THREAD_MULTIPLE not set, you may need to recompile your "
        << "MPI distribution with threads enabled" << std::endl;
    }
    else {
      std::cout << "MPI_THREAD_MULTIPLE is OK" << std::endl;
    }
  }

  //
  // Pause for debugging
  //
#ifdef H5FD_TEST_WAIT
  if (rank == 0) {
    std::cout << "Attach debugger if necessary, then press <enter>" << std::endl;
    char c;
    std::cin >> c;
  }
#endif

  if (argc > 1) {
    dsmSize = atol(argv[1]);
  }

  if (argc > 2) {
    if (!strcmp(argv[2], "Socket")) {
      commType = H5FD_DSM_COMM_SOCKET;
      if (rank == 0) std::cout << "SOCKET Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "MPI")) {
      commType = H5FD_DSM_COMM_MPI;
      if (rank == 0) std::cout << "MPI Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "MPI_RMA")) {
      commType = H5FD_DSM_COMM_MPI_RMA;
      if (rank == 0) std::cout << "MPI_RMA Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "DMAPP")) {
      commType = H5FD_DSM_COMM_DMAPP;
      staticInterComm = true;
      if (rank == 0) std::cout << "DMAPP Inter-Communicator selected" << std::endl;
    }
  }

  if (argc > 3) {
    if (!strcmp(argv[3], "Block")) {
      dsmType = H5FD_DSM_TYPE_BLOCK_CYCLIC;
    }
    else if (!strcmp(argv[3], "Static") && (commType != H5FD_DSM_COMM_SOCKET)) {
      staticInterComm = true;
    }
  }

  if (staticInterComm) {
    H5FDdsmInt32 color = 1; // 1 for server, 2 for client
    MPI_Comm_split(MPI_COMM_WORLD, color, rank, &comm);
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &size);
  }

  if (argc > 4) {
    dsmBlockSize = atol(argv[4]);
  }

  //std::cout << "Process number " << rank << " of " << size - 1 << std::endl;

  //
  // Create a DSM manager
  //
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  dsmManager->SetCommunicator(comm);
  dsmManager->SetLocalBufferSizeMBytes(dsmSize/size);
  dsmManager->SetDsmType(dsmType);
  dsmManager->SetDsmBlockLength(dsmBlockSize);
  dsmManager->SetDsmCommType(commType);
  dsmManager->SetDsmIsServer(1);
  if (staticInterComm) dsmManager->SetDsmUseStaticInterComm(1);
  dsmManager->SetServerHostName("default");
  dsmManager->SetServerPort(22000);
  dsmManager->CreateDSM();

  if (staticInterComm) {
    dsmManager->ConnectInterCommDSM();
  } else {
    // Publish writes .dsm_config file with server name/port/mode in
    // then spawns thread which waits for incoming connections
    dsmManager->PublishDSM();
  }
  //
  H5FDdsmFloat64 totalMB = (H5FDdsmFloat64) (dsmManager->GetDSMHandle()->GetTotalLength()/(1024*1024));
  H5FDdsmUInt32 serversize = (dsmManager->GetDSMHandle()->GetEndServerId() -
      dsmManager->GetDSMHandle()->GetStartServerId() + 1);
  if (rank == 0) {
    std::cout << "DSM server memory size is : " << totalMB << " MB" << std::endl;
    std::cout << "DSM server process count  : " <<  serversize << std::endl;
  }

  // The output comment below must not be deleted, it allows ctest to detect
  // when the server is initialized
  MPI_Barrier(comm);
  if (rank == 0) std::cout << "Waiting for client..." << std::endl;
  dsmManager->WaitForConnected();

  while(dsmManager->GetDsmIsConnected()) {
    if (dsmManager->WaitForUpdateReady() > 0) {
      // H5Dump
      // dsmManager->H5DumpLight();
      // Sync here
      MPI_Barrier(comm);
      // Clean up for next step
      dsmManager->UpdateFinalize();
    }
  }

  // std::cout << "Process number " << rank << " Closing down DSM server" << std::endl;

  //
  // Sync here
  //
  MPI_Barrier(comm);

  //
  // Closes ports or MPI communicators
  //
  if (!staticInterComm) dsmManager->UnpublishDSM();

  //
  // Clean up everything
  // 
  delete dsmManager;

  if (staticInterComm) {
   MPI_Comm_free(&comm);
  }

  MPI_Finalize();
  return(EXIT_SUCCESS);
}
