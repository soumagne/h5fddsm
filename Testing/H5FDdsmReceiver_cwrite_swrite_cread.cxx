#include "H5FDdsmTest.h"
#include "H5FDdsm.h"
#include "H5FDdsmTools.h"
//
#include <hdf5.h>
#include <cstdlib>

//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  MPI_Comm comm = MPI_COMM_WORLD;
  receiverInit(argc, argv, dsmManager, &comm);

  if (dsmManager->WaitForUnlock() != H5FD_DSM_FAIL) {
    int array[3] = { 1, 2, 3 };
    hsize_t arraySize = 3;

    H5FD_dsm_lock();
    H5FD_dsm_dump();
    // Sync here
    MPI_Barrier(comm);
    // Add another group/array
    hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_dsm(fapl, comm, NULL, 0);
    hid_t hdf5Handle = H5Fopen("dsm", H5F_ACC_RDWR, fapl);
    H5Pclose(fapl);
    hid_t memspace = H5Screate_simple(1, &arraySize, NULL);
    hid_t dataset = H5Dcreate(hdf5Handle, "Server_vector", H5T_NATIVE_INT, memspace,
        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, array);
    H5Sclose(memspace);
    H5Dclose(dataset);
    H5Fclose(hdf5Handle);
    H5FD_dsm_unlock(H5FD_DSM_NOTIFY_NONE);
  }

  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
