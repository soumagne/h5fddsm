#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif
//
#include <mpi.h>
#include <hdf5.h>
//
#include "H5FDdsm.h"

int main(int argc, char **argv)
{
  int array[3] = { 1, 2, 3 };
  hsize_t array_size = 3;
  int rank;
  int static_intercomm = 0;
  MPI_Comm comm = MPI_COMM_WORLD;

  /* Initialize MPI */
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(comm, &rank);

  if (argc > 3) {
    if (!strcmp(argv[3], "Static")) {
      static_intercomm = 1;
    }
  }

  if (static_intercomm) {
    int color = 2; // 1 for server, 2 for client
    MPI_Comm_split(MPI_COMM_WORLD, color, rank, &comm);
    MPI_Comm_rank(comm, &rank);
    /*
     * TODO Use sleep for now to be sure that the config file is created
     */
#ifdef _WIN32
        Sleep(1000);
#else
        sleep(1);
#endif
  }

  /* Create DSM file access property list and file */
  hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);
  H5Pset_fapl_dsm(fapl, comm, NULL, 0);
  hid_t file_handle = H5Fcreate("dsm", H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
  H5Pclose(fapl);

  /* Create dataspace and write data */
  hid_t memspace = H5Screate_simple(1, &array_size, NULL);
  hid_t dataset = H5Dcreate(file_handle, "Client_vector", H5T_NATIVE_INT, memspace,
      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  if (rank == 0)
    H5Dwrite(dataset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, array);
  H5Sclose(memspace);
  H5Dclose(dataset);

  /* Close file and shutdown HDF library */
  H5Fclose(file_handle);
  H5close();

  MPI_Finalize();
  return(EXIT_SUCCESS);
}
