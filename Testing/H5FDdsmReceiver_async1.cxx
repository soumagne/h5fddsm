#include "H5FDdsmTest.h"
#include "H5FDdsmSteering.h"
#include "H5FDdsmSteerer.h"
#include "H5FDdsm.h"
#include "H5FDdsmTools.h"

#include <cstdlib>

// sleep
#ifdef _WIN32
 #include "windows.h"
 #define sleep(a) Sleep(static_cast<int>(1000*a))
#else
 #include <unistd.h>
#endif

//----------------------------------------------------------------------------
H5FDdsmInt32 Iterations = 1;
H5FDdsmInt32 WaitSignal = 1;
//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  MPI_Comm comm = MPI_COMM_WORLD;
//  H5FDdsmFloat64 remoteMB;
//  H5FDdsmUInt64 numParticles;
  int iterations = 0;
  //
  receiverInit(argc, argv, dsmManager, &comm);

//  remoteMB = dsmManager->GetDsmBuffer()->GetTotalLength() / (1024.0 * 1024.0);
//  numParticles = (H5FDdsmUInt64) ((1024 * 1024 * remoteMB / 2) /
//      (sizeof(H5FDdsmFloat64) * DIM_DATASETS * dsmManager->GetUpdateNumPieces()));

//  int intScalar = 0;

  // wait until data has been written
  while (dsmManager->WaitForUnlock() != H5FD_DSM_FAIL) {
    //
    // manually lock (acquire) the DSM
    //
    H5FD_dsm_lock();
    H5FD_dsm_dump();

    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Checking data at iteration " << iterations << std::endl;
    }
    /*
      H5FDdsmBoolean present;
      dsmManager->GetSteerer()->IsObjectPresent("IntScalarTest", present);
      if (present) {
        std::cout << "\n\nReceiver found " << intScalar << std::endl << std::endl;
        dsmManager->GetSteeringValues("IntScalarTest", 1, &intScalar);
      }
      intScalar++;
      dsmManager->SetSteeringValues("IntScalarTest", 1, &intScalar);
      std::cout << "\n\n\nReceiver Written " << intScalar << std::endl << std::endl;


      if (dsmManager->GetUpdatePiece() == 0) {
        std::cout << "Checked data at iteration " << iterations << std::endl;
      }
     */
    //
    // manually unlock (release) the DSM and send a NEW_DATA message
    //
    H5FD_dsm_unlock(H5FD_DSM_NOTIFY_DATA);

    //      dsmManager->SetSteeringCommand("play");

    iterations++;
  }

  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}

/*
      // Simulate control taken by an arbitrary app, modifies values and
      // give back hand by sending a play
//      dsmManager->SetSteeringValues("WaitSignal", 1, &WaitSignal);
//      dsmManager->SetSteeringValues("Iterations", 1, &Iterations);
*/
