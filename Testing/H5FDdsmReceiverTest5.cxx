#include <H5FDdsmManager.h>
#include <hdf5.h>
#include <mpi.h>

#include <iostream>
#include <cstdlib>

// Sleep  in milliseconds
#ifdef _WIN32
  #include <windows.h>
  #define sleep(a) ::Sleep(a)
#else
  void mySleep(int ms) {
    usleep(ms*1000); //convert to microseconds
    return;
  }
  #define sleep(a) mySleep(a)
#endif

int
main(int argc, char * argv[])
{
  int rank, size, provided;
  MPI_Comm comm = MPI_COMM_WORLD;
  long dsmSize = 16; // default MB
  int commType = H5FD_DSM_COMM_SOCKET;

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

  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
  MPI_Comm_rank(comm, &rank);
  if (rank == 0) {
    if (provided != MPI_THREAD_MULTIPLE) {
      std::cout << "MPI_THREAD_MULTIPLE not set, you may need to recompile your "
        << "MPI distribution with threads enabled" << std::endl;
    }
    else {
      std::cout << "MPI_THREAD_MULTIPLE is OK" << std::endl;
    }
  }
  MPI_Comm_size(comm, &size);
  std::cout << "Process number " << rank << " of " << size << std::endl;

  // Publish DSM Server
  H5FDdsmManager * dsmManager = new H5FDdsmManager();
  dsmManager->SetGlobalDebug(0);
  dsmManager->SetCommunicator(comm);
  dsmManager->SetLocalBufferSizeMBytes(dsmSize);
  dsmManager->SetDsmCommType(commType);
  dsmManager->SetDsmIsServer(1);
  dsmManager->SetServerHostName("default");
  dsmManager->SetServerPort(22000);
  dsmManager->CreateDSM();
  dsmManager->PublishDSM();

  int totalMB = static_cast<int> (dsmManager->GetDSMHandle()->GetTotalLength()
      / (1024 * 1024));
  int serversize = dsmManager->GetDSMHandle()->GetEndServerId();
  if (rank == 0) {
    std::cout << "DSM server memory size is : " << totalMB << " MB"
        << std::endl;
    std::cout << "DSM server process count  : " << (serversize + 1)
              << std::endl;
  }

  // The output comment below must not be deleted, it allows ctest to detect
  // when the server is initialized
  sleep(100);
  std::cout << "Waiting for client..." << std::endl;
  dsmManager->WaitForConnected();

  while (dsmManager->GetDSMHandle()->GetIsConnected()) {
    if (dsmManager->WaitForUpdateReady() > 0) {
      // H5Dump
      dsmManager->H5DumpLight();
      // Sync here
      MPI_Barrier(comm);
      // Clean up for next step
      dsmManager->UpdateFinalize();
    }
  }

  std::cout << "Process number " << rank << " Closing down DSM server"
      << std::endl;

  // Sync here
  MPI_Barrier(comm);

  // Closes ports or MPI communicators
  dsmManager->UnpublishDSM();

  // Clean up everything
  delete dsmManager;

  MPI_Finalize();

  return 0;
}
