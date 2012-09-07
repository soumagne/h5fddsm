#include "H5FDdsmTest.h"

#include <cstdlib>
#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif


//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  MPI_Comm comm = MPI_COMM_WORLD;
  H5FDdsmInt32 provided, rank, size;
  H5FDdsmUInt32 dsmSize = 16; // default MB
  H5FDdsmInt32 commType = H5FD_DSM_COMM_SOCKET;
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
      std::cout << "# MPI_THREAD_MULTIPLE not set, you may need to recompile your "
          << "MPI distribution with threads enabled" << std::endl;
    }
    else {
      std::cout << "# MPI_THREAD_MULTIPLE is OK" << std::endl;
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
  //
  if (argc > 1) {
    if (!strcmp(argv[1], "Socket")) {
      commType = H5FD_DSM_COMM_SOCKET;
      if (rank == 0) std::cout << "# SOCKET Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[1], "MPI")) {
      commType = H5FD_DSM_COMM_MPI;
      if (rank == 0) std::cout << "# MPI Inter-Communicator selected" << std::endl;
    }
  }
  // Create a DSM manager
  dsmManager->SetMpiComm(comm);
  dsmManager->SetLocalBufferSizeMBytes(dsmSize/size);
  dsmManager->SetInterCommType(commType);
  dsmManager->SetIsServer(H5FD_DSM_TRUE);
  dsmManager->SetServerHostName("default");
  dsmManager->SetServerPort(22000);
  dsmManager->Create();
  dsmManager->Publish();
  //
  MPI_Barrier(comm);
  if (rank == 0) std::cout << "# Waiting for client..." << std::endl;
  /*
   * TODO Use sleep for now to be sure that we've entered accept
   */
#ifdef _WIN32
  Sleep(1000);
#else
  sleep(1);
#endif
  //
  delete dsmManager;
  //
  MPI_Finalize();
  //
  return(EXIT_SUCCESS);
}
