#include "H5FDdsmTest.h"
#include "H5FDdsm.h"
//
#include <hdf5.h>
#include <cstdlib>

//----------------------------------------------------------------------------
int main(int argc, char * argv[])
{
  MPI_Comm comm = MPI_COMM_WORLD;
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  senderInit(argc, argv, dsmManager, &comm);

  H5FDdsmBuffer *dsmBuffer = dsmManager->GetDSMHandle();

  // Create Array
  int array1[3] = { 1, 2, 3 };
  int array2[6] = { 4, 5, 6, 7, 8, 9 };

  hsize_t arraySize1 = 3;
  hsize_t arraySize2 = 6;

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

  std::cout << "Attempt to add another data set" << std::endl;

  // Set up file access property list with parallel I/O
  fapl = H5Pcreate(H5P_FILE_ACCESS);

  H5Pset_fapl_dsm(fapl, MPI_COMM_WORLD, dsmManager->GetDSMHandle());

  hdf5Handle = H5Fopen("dsm", H5F_ACC_RDWR, fapl);

  // Close property list
  H5Pclose(fapl);

  memspace = H5Screate_simple(1, &arraySize2, NULL);
  dataset = H5Dcreate(hdf5Handle, "Data1", H5T_NATIVE_INT, memspace,
      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

  H5Dwrite(dataset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, array2);

  H5Sclose(memspace);
  H5Dclose(dataset);
  H5Fclose(hdf5Handle);

  senderFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
