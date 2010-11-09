#include <cstdlib>
#include <cstring>
#include <iostream>
#include <mpi.h>
#include <hdf5.h>
#include <H5FDdsm.h>

using std::cerr;
using std::cout;
using std::endl;

//#define JDEBUG
#ifdef  JDEBUG
#  define PRINT_DEBUG_INFO(x) cout << x << endl;
#else
#  define PRINT_DEBUG_INFO(x)
#endif
#define PRINT_INFO(x) cout << x << endl;
#define PRINT_ERROR(x) cerr << x << endl;

//----------------------------------------------------------------------------

int
main(int argc, char *argv[])
{
  hid_t           file_id, group_id, dataset_id1, dataspace_id1, dataset_id2, dataspace_id2, fapl;  /* identifiers */
  hsize_t         dims[2];
  int             dset1_data[3][3], dset2_data[2][10];
  int             rank, size;
  int             dsm = 1;
  int             nwrite = 1;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

#ifdef WIN32
  // wait for input char to allow debugger to be connected
  if (rank==0) {
    char ch;
    std::cin >> ch;
  }
  MPI_Barrier(MPI_COMM_WORLD);
#endif

  if (size != 2) {
    if (rank == 0) PRINT_INFO("Must be run with 2 processes");
    MPI_Finalize();
    return EXIT_FAILURE;
  }

  // Initialize the first dataset
  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 3; j++)
      dset1_data[i][j] = j + 1;

  // Initialize the second dataset
  for (int i = 0; i < 2; i++)
    for (int j = 0; j < 10; j++)
      dset2_data[i][j] = j + 1;

  for(int write_step = 0; write_step < nwrite; write_step++) {

    PRINT_DEBUG_INFO(endl << "------ Writing step " << write_step << " ------" << endl);
    // Create the file access property list
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    if (dsm == 1) {
      H5Pset_fapl_dsm(fapl, MPI_COMM_WORLD, NULL);
    } else {
      PRINT_INFO("Using Parallel File Interface");
      H5Pset_fapl_mpio(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);
    }

    // Create a new file
    if (dsm == 1) {
      PRINT_DEBUG_INFO("Writing data to DSM...");
    } else {
      PRINT_DEBUG_INFO("Writing data to disk...");
    }

    file_id = H5Fcreate("H5FDdsmTest.h5", H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    PRINT_DEBUG_INFO(endl << "Create file");
    if (file_id < 0) {
      PRINT_ERROR("Cannot create file");
      return EXIT_FAILURE;
    }

    // Close the file access property list
    H5Pclose(fapl);

    // Create the data space for the first dataset
    PRINT_DEBUG_INFO(endl << "Create the first dataspace");
    dims[0] = 3;
    dims[1] = 3;
    dataspace_id1 = H5Screate_simple(2, dims, NULL);

    // Create a dataset in group "/"
    PRINT_DEBUG_INFO("Create the first dataset");
    dataset_id1 = H5Dcreate(file_id, "/dset1", H5T_STD_I32BE, dataspace_id1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    // Create the group MyGroup
    PRINT_DEBUG_INFO(endl << "Create the group");
    group_id = H5Gcreate(file_id, "/MyGroup", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    // Create the data space for the second dataset
    PRINT_DEBUG_INFO("Create the second dataspace");
    dims[0] = 2;
    dims[1] = 10;
    dataspace_id2 = H5Screate_simple(2, dims, NULL);

    // Create the second dataset in group "/MyGroup"
    PRINT_DEBUG_INFO("Create the second dataset");
    dataset_id2 = H5Dcreate(group_id, "dset2", H5T_STD_I32BE, dataspace_id2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    // Write the first dataset
    if (rank == 0) {
      PRINT_DEBUG_INFO(endl << "Write the first dataset");
      H5Dwrite(dataset_id1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1_data);
    }

    // Write the second dataset
    if (rank == 1) {
      PRINT_DEBUG_INFO(endl << "Write the second dataset");
      H5Dwrite(dataset_id2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_data);
    }

    // Close the data space for the first dataset
    PRINT_DEBUG_INFO(endl << "Close datasets");
    H5Sclose(dataspace_id1);

    // Close the first dataset
    H5Dclose(dataset_id1);

    // Close the data space for the second dataset
    H5Sclose(dataspace_id2);

    // Close the second dataset
    H5Dclose(dataset_id2);

    // Close the group
    PRINT_DEBUG_INFO("Close group");
    H5Gclose(group_id);

    // Close the file
    PRINT_DEBUG_INFO("Close file");
    H5Fclose(file_id);
  }

  PRINT_INFO("Now exiting...");
  PRINT_DEBUG_INFO("About to MPI_Finalize");
  MPI_Barrier(MPI_COMM_WORLD);
  H5close();
  MPI_Finalize();
  return EXIT_SUCCESS;
}
