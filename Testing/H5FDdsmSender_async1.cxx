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
int main(int argc, char **argv)
{
  MPI_Comm comm = MPI_COMM_WORLD;
  H5FDdsmFloat64 remoteMB;
  H5FDdsmUInt64 numParticles;
  H5FDdsmConstString fullname = "dsm";
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  //
  senderInit(argc, argv, dsmManager, &comm);
  //
  H5FD_dsm_steering_init(comm);
  //
  remoteMB = dsmManager->GetDsmBuffer()->GetTotalLength() / (1024.0 * 1024.0);
  numParticles = (H5FDdsmUInt64) ((1024 * 1024 * remoteMB / 2) /
      (sizeof(H5FDdsmFloat64) * DIM_DATASETS * dsmManager->GetUpdateNumPieces()));
  //
  // our fake simulation will do N iterations
  //
  for (int i=0; i<5; i++) {
    // Disable auto notification to allow manual trigger
    H5FD_dsm_set_options(H5FD_DSM_DONT_RELEASE);

    //
    // open/write/close using a standard test particle dataset
    //
    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Writing data at iteration " << i << std::endl;
    }
    TestParticleWrite(fullname, numParticles, DIM_DATASETS, NUM_DATASETS, dsmManager->GetUpdatePiece(),
        dsmManager->GetUpdateNumPieces(), comm, dsmManager, usingHDF);
    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Written data at iteration " << i << std::endl;
    }

    //
    // manually unlock (release) the DSM and send a NEW_DATA message
    //
    H5FD_dsm_notify(H5FD_DSM_NEW_DATA);
    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Notification sent at iteration " << i << std::endl;
    }
//    sleep(1);

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
