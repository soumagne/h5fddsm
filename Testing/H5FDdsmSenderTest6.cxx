#include <hdf5.h>
#include <H5FDdsmManager.h>
#include <H5FDdsm.h>
#include <mpi.h>

#include <iostream>

int
main(int argc, char * argv[])
{
  MPI_Init(&argc, &argv);

  int rank, size;

  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  // Create DSM Client
  H5FDdsmManager * dsmManager = new H5FDdsmManager();
  dsmManager->SetGlobalDebug(0);
  dsmManager->SetCommunicator(comm);
  dsmManager->SetDsmIsServer(0);
  dsmManager->ReadDSMConfigFile();
  dsmManager->CreateDSM();

  // Connect to Server
  dsmManager->ConnectDSM(true);

  H5FDdsmBuffer * dsmBuffer = dsmManager->GetDSMHandle();

  // Get info from remote server
  double remoteMB = dsmBuffer->GetTotalLength() / (1024.0 * 1024.0);
  double numServers = dsmBuffer->GetEndServerId() + 1;
  if (rank == 0) {
    std::cout << "DSM server memory size is : " << (int) remoteMB << " MB"
        << std::endl;
    std::cout << "DSM server process count  : " << (int) numServers
        << std::endl;
  }

  // Create Array
  int array1[3] = { 1, 2, 3 };

  hsize_t arraySize1 = 3;

  // Set file access property list for DSM
  hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);

  // Use DSM driver
  H5Pset_fapl_dsm(fapl, MPI_COMM_WORLD, dsmBuffer);

  // Create DSM
  hid_t hdf5Handle = H5Fcreate("dsm", H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

  // Close file access property list
  H5Pclose(fapl);

  hid_t memspace = H5Screate_simple(1, &arraySize1, NULL);
  hid_t dataset = H5Dcreate(hdf5Handle, "Data0", H5T_NATIVE_INT, memspace,
      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

  H5Dwrite(dataset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, array1);

  H5Sclose(memspace);
  H5Dclose(dataset);

  H5Fclose(hdf5Handle);

  // Sync here
  MPI_Barrier(comm);

  dsmManager->H5DumpLight();

  // Sync here
  MPI_Barrier(comm);

  dsmManager->DisconnectDSM();

  delete dsmManager;

  MPI_Finalize();

  return 0;
}
