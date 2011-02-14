#include <H5FDdsmManager.h>
#include <H5FDdsm.h>
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
  long DSMSize = 16; // default MB
  int commType = H5FD_DSM_COMM_SOCKET;

  if (argc > 1) {
    DSMSize = atol(argv[1]);
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
  dsmManager->SetLocalBufferSizeMBytes(DSMSize);
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

  sleep(100);
  std::cout << "Waiting for client..." << std::endl;
  while (!dsmManager->GetDSMHandle()->GetIsConnected()) {
    sleep(1000);
  }

  bool connected = true;
  while (connected) {
    dsmManager->WaitForUpdateReady();
    int array[3] = { 1, 2, 3 };
    hsize_t arraySize = 3;

    // H5Dump
    dsmManager->H5DumpLight();

    // Sync here
    MPI_Barrier(comm);

    // Add another group/array
    hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_dsm(fapl, MPI_COMM_WORLD, dsmManager->GetDSMHandle());
    hid_t hdf5Handle = H5Fopen("dsm", H5F_ACC_RDWR, fapl);
    H5Pclose(fapl);
    hid_t memspace = H5Screate_simple(1, &arraySize, NULL);
    hid_t dataset = H5Dcreate(hdf5Handle, "Server_vector", H5T_NATIVE_INT, memspace,
        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, array);
    H5Sclose(memspace);
    H5Dclose(dataset);
    H5Fclose(hdf5Handle);

    // Clean up for next step
    dsmManager->UpdateFinalize();
    connected = (dsmManager->GetDSMHandle()->GetIsConnected() != 0);
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
