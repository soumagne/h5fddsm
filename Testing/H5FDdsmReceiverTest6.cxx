#include <H5FDdsmManager.h>
#include <H5FDdsm.h>
#include <hdf5.h>
#include <mpi.h>

#include <iostream>
#include <cstdlib>

int
main(int argc, char * argv[])
{
  int rank, size, provided;
  MPI_Comm comm = MPI_COMM_WORLD;
  long dsmSize = 16; // default MB
  int commType = H5FD_DSM_COMM_SOCKET;
  bool staticInterComm = false;

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
    else if (!strcmp(argv[2], "DMAPP")) {
      commType = H5FD_DSM_COMM_DMAPP;
      staticInterComm = true;
      if (rank == 0) std::cout << "DMAPP Inter-Communicator selected" << std::endl;
    }
  }

  if (argc > 3) {
    if (!strcmp(argv[3], "Static") && (commType != H5FD_DSM_COMM_SOCKET)) {
      staticInterComm = true;
    }
  }

  if (staticInterComm) {
    int color = 1; // 1 for server, 2 for client
    MPI_Comm_split(MPI_COMM_WORLD, color, rank, &comm);
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &size);
  }

  std::cout << "Process number " << rank << " of " << size - 1 << std::endl;

  // Publish DSM Server
  H5FDdsmManager * dsmManager = new H5FDdsmManager();
  dsmManager->SetGlobalDebug(0);
  dsmManager->SetCommunicator(comm);
  dsmManager->SetLocalBufferSizeMBytes(dsmSize);
  dsmManager->SetDsmCommType(commType);
  dsmManager->SetDsmIsServer(1);
  if (staticInterComm) dsmManager->SetDsmUseStaticInterComm(1);
  dsmManager->SetServerHostName("default");
  dsmManager->SetServerPort(22000);
  dsmManager->CreateDSM();

  if (staticInterComm) {
    dsmManager->ConnectInterCommDSM();
  } else {
    dsmManager->PublishDSM();
  }

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
  MPI_Barrier(comm);
  std::cout << "Waiting for client..." << std::endl;
  dsmManager->WaitForConnected();

  while (dsmManager->GetDSMHandle()->GetIsConnected()) {
    if (dsmManager->WaitForUpdateReady() > 0) {
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
    }
  }

  std::cout << "Process number " << rank << " Closing down DSM server"
      << std::endl;

  // Sync here
  MPI_Barrier(comm);

  // Closes ports or MPI communicators
  if (!staticInterComm) dsmManager->UnpublishDSM();

  // Clean up everything
  delete dsmManager;

  if (staticInterComm) {
   MPI_Comm_free(&comm);
  }

  MPI_Finalize();

  return 0;
}
