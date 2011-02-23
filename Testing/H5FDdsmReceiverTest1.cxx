// DSM features
#include "H5FDdsmManager.h"

// Sys
#include <cstdlib>
#include <sstream>
#include <mpi.h>

//----------------------------------------------------------------------------
int main (int argc, char* argv[])
{
  int provided, rank, size;
  MPI_Comm dcomm = MPI_COMM_WORLD;
  long dsmSize = 16; // default MB
  int commType = H5FD_DSM_COMM_SOCKET;
  int dsmType = H5FD_DSM_TYPE_UNIFORM;
  long dsmBlockSize = 1024;

  if (argc > 1) {
    dsmSize = atol(argv[1]);
  }

  if (argc > 2) {
    if (!strcmp(argv[2], "Socket")) {
      commType = H5FD_DSM_COMM_SOCKET;
      std::cout << "SOCKET Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "MPI")) {
      commType = H5FD_DSM_COMM_MPI;
      std::cout << "MPI Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "MPI_RMA")) {
      commType = H5FD_DSM_COMM_MPI_RMA;
      std::cout << "MPI_RMA Inter-Communicator selected" << std::endl;
    }
  }

  if (argc > 3) {
    if (!strcmp(argv[3], "Block")) {
      dsmType = H5FD_DSM_TYPE_BLOCK_CYCLIC;
    }
  }

  if (argc > 4) {
    dsmBlockSize = atol(argv[4]);
  }

  //
  // Receiver will spawn a thread to handle incoming data Put/Get requests
  // we must therefore have MPI_THREAD_MULTIPLE 
  //
  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
  MPI_Comm_rank(dcomm, &rank);
  MPI_Comm_size(dcomm, &size);
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
  std::cout << "Process number " << rank << " of " << size << std::endl;

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

  //
  // Create a DSM manager
  //
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  dsmManager->SetCommunicator(dcomm);
  dsmManager->SetLocalBufferSizeMBytes(dsmSize/size);
  dsmManager->SetDsmType(dsmType);
  dsmManager->SetDsmBlockLength(dsmBlockSize);
  dsmManager->SetDsmCommType(commType);
  dsmManager->SetDsmIsServer(1);
  dsmManager->SetServerHostName("default");
  dsmManager->SetServerPort(22000);
  dsmManager->CreateDSM();
  // Publish writes .dsm_config file with server name/port/mode in
  // then spawns thread which waits for incoming connections
  dsmManager->PublishDSM();
  //
  int totalMB = static_cast<int>(dsmManager->GetDSMHandle()->GetTotalLength()/(1024*1024));
  int serversize = dsmManager->GetDSMHandle()->GetEndServerId();
  if (rank == 0) {
    std::cout << "DSM server memory size is : " << totalMB << " MB" << std::endl;
    std::cout << "DSM server process count  : " <<  (serversize+1) << std::endl;
  }

  // The output comment below must not be deleted, it allows ctest to detect
  // when the server is initialized
  MPI_Barrier(dcomm);
  std::cout << "Waiting for client..." << std::endl;
  dsmManager->WaitForConnected();

  while(dsmManager->GetDsmIsConnected()) {
    if (dsmManager->WaitForUpdateReady() > 0) {
      // H5Dump
      // dsmManager->H5DumpLight();
      // Sync here
      MPI_Barrier(dcomm);
      // Clean up for next step
      dsmManager->UpdateFinalize();
    }
  }

  std::cout << "Process number " << rank << " Closing down DSM server" << std::endl;

  //
  // Sync here
  //
  MPI_Barrier(dcomm);

  //
  // Closes ports or MPI communicators
  //
  dsmManager->UnpublishDSM();

  //
  // Clean up everything
  // 
  delete dsmManager;

  MPI_Finalize();
  return(EXIT_SUCCESS);
}
