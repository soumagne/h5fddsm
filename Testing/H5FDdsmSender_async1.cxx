#include "H5FDdsmTest.h"
#include "H5FDdsmSteering.h"
#include "H5FDdsmSteerer.h"
#include "H5FDdsm.h"
#include "H5FDdsmTools.h"
//
#include <hdf5.h>
#include <cstdlib>
#include <cassert>

// sleep
#ifdef _WIN32
 #include "windows.h"
 #define sleep(a) Sleep(1000*a)
#else
 #include <unistd.h>
#endif

//----------------------------------------------------------------------------
void writesmalldataset() {
  int array[3] = { 1, 2, 3 };
  hsize_t array_size = 3;
  int rank;
  int static_intercomm = 0;
  hid_t fapl, file_handle, memspace, dataset;
  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Comm_rank(comm, &rank);

  /* Create DSM file access property list and file */
  fapl = H5Pcreate(H5P_FILE_ACCESS);
  H5Pset_fapl_dsm(fapl, comm, NULL, 0);
  file_handle = H5Fcreate("dsm", H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
  H5Pclose(fapl);

  /* Create dataspace and write data */
  memspace = H5Screate_simple(1, &array_size, NULL);
  dataset = H5Dcreate(file_handle, "Client_vector", H5T_NATIVE_INT, memspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  if (rank == 0) {
    H5Dwrite(dataset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, array);
  }
  H5Sclose(memspace);
  H5Dclose(dataset);

  /* Close file and shutdown HDF library */
  H5Fclose(file_handle);
}
//----------------------------------------------------------------------------
int main(int argc, char **argv)
{
  MPI_Comm comm = MPI_COMM_WORLD;
  H5FDdsmConstString fullname = "dsm";
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  //
  senderInit(argc, argv, dsmManager, &comm);
  //
  H5FD_dsm_steering_init(comm);
  //
  // Disable auto unlock to allow manual trigger
//  H5FD_dsm_set_options(H5FD_DSM_UNLOCK_MANUAL);
//  H5FD_dsm_set_options(H5FD_DSM_LOCK_ASYNCHRONOUS);

  //
  // our fake simulation will do N iterations
  //
  int intScalar = 0;
  for (int i=0; i<5; i++) {
    //
    // open/write/close using a standard test particle dataset
    //
    //TestParticleWrite(fullname, numParticles, DIM_DATASETS, NUM_DATASETS, dsmManager->GetUpdatePiece(),
    //    dsmManager->GetUpdateNumPieces(), comm, dsmManager, usingHDF);
    //if (dsmManager->GetUpdatePiece() == 0) {
    //  std::cout << "Written data at iteration " << i << std::endl;
    //}

    //
    // manually lock (acquire) the DSM 
    //
    H5FD_dsm_lock();

    writesmalldataset();
/*
    H5FDdsmBoolean present;
    dsmManager->GetSteerer()->IsObjectPresent("IntScalarTest", present);
    if (present) {
      std::cout << "\n\nSender found " << intScalar << std::endl << std::endl;
      dsmManager->GetSteeringValues("IntScalarTest", 1, &intScalar);
    }
    intScalar++;
    dsmManager->SetSteeringValues("IntScalarTest", 1, &intScalar);
    std::cout << "\n\nSender Written " << intScalar << std::endl << std::endl;
*/
    //
    // manually unlock (release) the DSM and send a NEW_DATA message
    //
    H5FD_dsm_unlock(H5FD_DSM_NOTIFY_DATA);
    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Notification sent at iteration " << i << std::endl;
    }
  }


  senderFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}

/*
//    H5FD_dsm_steering_update();
//    if (dsmManager->GetUpdatePiece() == 0) {
//      std::cout << "completed H5FD_dsm_steering_update at iteration " << i << std::endl;
      //sleep(0.5);
//    }
*/

/*    
    // write out an iteration number using the steering API for convenience
    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Writing steering value from simulation at iteration " << i << std::endl;
    }
//    dsmManager->SetSteeringValues("Iteration", 1, &i);


    // Begin a steering query sequence for checking returned data
    H5FD_dsm_steering_begin_query();

    H5FDdsmBoolean Iterations=0;
    hbool_t waitflagpresent = false;
    H5FD_dsm_steering_is_set("WaitSignal", &waitflagpresent);
    if (waitflagpresent) {
      // After how many Iterations shall we do a wait 
      H5FD_dsm_steering_is_set("Iterations", &IterationsSet);
      if (IterationsSet) {
        H5FD_dsm_steering_scalar_get("Iterations", H5T_NATIVE_INT, &Iterations);
        if (dsmManager->GetUpdatePiece() == 0) {
//          std::cout << "Will wait in " << Iterations << " Iterations: " << std::endl;
        }
      }
    }

    // End steering query sequence
    H5FD_dsm_steering_end_query();

    // Do a wait if we were asked to
    if ((Iterations--) == 0) {
      // we now wait for an explicit resume command from the DSM server
      if (dsmManager->GetUpdatePiece() == 0) {
//        std::cout << "Waiting at end of iteration " << i << std::endl;
      }
//      H5FD_dsm_steering_wait();
    }
    */
