#include <cstdlib>
#include <cstring>
#include <iostream>
#include <mpi.h>
#include <hdf5.h>

#include "H5FDdsm.h"

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
  hid_t           file_id, group_id, dataset_id1, dataset_id2, fapl;  /* identifiers */
  int             dset1_data[3][3], dset2_data[2][10];
  int             rank, size;
  int             dsm = 1;
  int             nread = 1;
  int             fail_count = 0;

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
      dset1_data[i][j] = 0;

  // Initialize the second dataset
  for (int i = 0; i < 2; i++)
    for (int j = 0; j < 10; j++)
      dset2_data[i][j] = 0;

  for(int read_step = 0; read_step < nread; read_step++) {

    PRINT_DEBUG_INFO(endl << "------ Writing step " << read_step << " ------" << endl);
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

    file_id = H5Fopen("H5FDdsmTest.h5", H5F_ACC_RDONLY, fapl);
    PRINT_DEBUG_INFO(endl << "Open file");
    if (file_id < 0) {
      PRINT_ERROR("Cannot open file");
      return EXIT_FAILURE;
    }

    // Close the file access property list
    H5Pclose(fapl);

    // Open the first dataset in group "/"
    PRINT_DEBUG_INFO("Open the first dataset");
    dataset_id1 = H5Dopen(file_id, "/dset1", H5P_DEFAULT);

    // Open the group MyGroup
    PRINT_DEBUG_INFO(endl << "Open the group");
    group_id = H5Gopen(file_id, "/MyGroup", H5P_DEFAULT);

    // Open the second dataset in group "/MyGroup"
    PRINT_DEBUG_INFO("Open the second dataset");
    dataset_id2 = H5Dopen(group_id, "dset2", H5P_DEFAULT);

    // Read the first dataset
    if (rank == 0) {
      PRINT_DEBUG_INFO(endl << "Read the first dataset");
      H5Dread(dataset_id1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1_data);
    }

    // Read the second dataset
    if (rank == 1) {
      PRINT_DEBUG_INFO(endl << "Read the second dataset");
      H5Dread(dataset_id2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_data);
    }

    PRINT_DEBUG_INFO(endl << "Close datasets");
    // Close the first dataset
    H5Dclose(dataset_id1);

    // Close the second dataset
    H5Dclose(dataset_id2);

    // Close the group
    PRINT_DEBUG_INFO("Close group");
    H5Gclose(group_id);

    // Close the file
    PRINT_DEBUG_INFO("Close file");
    H5Fclose(file_id);

    // Check the datasets
    if (rank == 0) {
      /* Check the results. */
      for (int i = 0; i < 3; i++)
        for (int j = 0; j < 3; j++)
          if((dset1_data[i][j] != (j+1)) && (fail_count<10)) {
            fprintf(stderr,"dset1_data[%d][%d] is %d, should be %d\n", i, j, dset1_data[i][j], j+1);
            fail_count++;
          }

      if (fail_count == 0) {
        printf("DSM read test PASSED for dset1_data\n");
      }
      else {
        fprintf(stderr,"DSM read FAILED for dset1_data, %d or more wrong values\n", fail_count);
      }
    }

    if (rank == 1) {
      /* Check the results. */
      for (int i = 0; i < 2; i++)
        for (int j = 0; j < 10; j++)
          if((dset2_data[i][j] != (j+1)) && (fail_count<10)) {
            fprintf(stderr,"dset2_data[%d][%d] is %d, should be %d\n", i, j, dset2_data[i][j], j+1);
            fail_count++;
          }

      if (fail_count == 0) {
        printf("DSM read test PASSED for dset2_data\n");
      }
      else {
        fprintf(stderr,"DSM read FAILED for dset2_data, %d or more wrong values\n", fail_count);
      }
    }
  }

  PRINT_INFO("Now exiting...");
  PRINT_DEBUG_INFO("About to MPI_Finalize");
  MPI_Barrier(MPI_COMM_WORLD);
  H5close();
  MPI_Finalize();
  return EXIT_SUCCESS;
}
