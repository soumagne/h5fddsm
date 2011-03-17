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
  bool staticInterComm = false;

  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  // Create DSM Client
  H5FDdsmManager * dsmManager = new H5FDdsmManager();
  dsmManager->ReadDSMConfigFile();
  if (dsmManager->GetDsmUseStaticInterComm()) {
    int color = 2; // 1 for server, 2 for client
    MPI_Comm_split(MPI_COMM_WORLD, color, rank, &comm);
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &size);
    staticInterComm = true;
  }
  dsmManager->SetCommunicator(comm);
  dsmManager->SetDsmIsServer(0);
  dsmManager->CreateDSM();

  if (staticInterComm) {
    dsmManager->ConnectInterCommDSM();
  } else {
    // Connect to Server
    dsmManager->ConnectDSM(true);
  }

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
  int array[3] = { 1, 2, 3 };
  int read_array[3];
  hsize_t arraySize = 3;

  // Set file access property list for DSM
  hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);

  // Use DSM driver
  H5Pset_fapl_dsm(fapl, MPI_COMM_WORLD, dsmBuffer);
  H5FD_dsm_set_mode(H5FD_DSM_MANUAL_SERVER_UPDATE, dsmBuffer);

  // Create DSM
  hid_t hdf5Handle = H5Fcreate("dsm", H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

  // Close file access property list
  H5Pclose(fapl);

  hid_t memspace = H5Screate_simple(1, &arraySize, NULL);
  hid_t dataset = H5Dcreate(hdf5Handle, "Data0", H5T_NATIVE_INT, memspace,
      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hid_t dataspace = H5S_ALL;

  H5Dwrite(dataset, H5T_NATIVE_INT, memspace, dataspace, H5P_DEFAULT, array);
  if (dataspace != H5S_ALL) {
    H5Sclose(dataspace);
  }
  if (memspace != H5S_ALL) {
    H5Sclose(memspace);
  }
  H5Dclose(dataset);

  H5Fclose(hdf5Handle);

  std::cout << "Attempt to Read Data" << std::endl;

  // Set up file access property list with parallel I/O
  fapl = H5Pcreate(H5P_FILE_ACCESS);

  H5Pset_fapl_dsm(fapl, MPI_COMM_WORLD, dsmManager->GetDSMHandle());

  hdf5Handle = H5Fopen("dsm", H5F_ACC_RDONLY, fapl);

  // Close property list
  H5Pclose(fapl);

  dataset = H5Dopen(hdf5Handle, "Data0", H5P_DEFAULT);
  dataspace = H5Dget_space(dataset);
  hssize_t numVals = H5Sget_simple_extent_npoints(dataspace);
  hid_t datatype = H5Dget_type(dataset);

  std::cout << "Number of Values Read: " << numVals << std::endl;
  H5Dread(dataset, datatype, H5S_ALL, dataspace, H5P_DEFAULT, &read_array);
  for(unsigned int i=0; i<numVals; ++i) {
    if (array[i] != read_array[i]) {
      fprintf(stderr," read_array[%d] is %d, should be %d\n", i, read_array[i], array[i]);
    }
  }

  H5Tclose(datatype);
  H5Sclose(dataspace);
  H5Dclose(dataset);
  H5Fclose(hdf5Handle);
  H5FD_dsm_server_update(dsmBuffer);

  dsmManager->DisconnectDSM();

  delete dsmManager;

  if (staticInterComm) {
   MPI_Comm_free(&comm);
  }

  MPI_Finalize();

  return 0;
}
