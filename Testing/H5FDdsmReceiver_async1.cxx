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
  H5FDdsmFloat64 remoteMB;
  H5FDdsmUInt64 numParticles;
  int iterations = 0;
  //
  receiverInit(argc, argv, dsmManager, &comm);

  remoteMB = dsmManager->GetDsmBuffer()->GetTotalLength() / (1024.0 * 1024.0);
  numParticles = (H5FDdsmUInt64) ((1024 * 1024 * remoteMB / 2) /
      (sizeof(H5FDdsmFloat64) * DIM_DATASETS * dsmManager->GetUpdateNumPieces()));

//  sleep(0.25);

  while (dsmManager->GetIsConnected()) {

    // wait until data has been written
    if (dsmManager->WaitForNotification() > 0) {

      // See what's in there
      for (int i=0; i<1; i++) {
        if (dsmManager->GetUpdatePiece() == 0) {
          std::cout << "Checking data at iteration " << iterations << std::endl;
          //sleep(0.5);
        }
        TestParticleRead("dsm", numParticles, DIM_DATASETS, NUM_DATASETS,
          dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(),
          comm, dsmManager, false);
        if (dsmManager->GetUpdatePiece() == 0) {
          std::cout << "Checked data at iteration " << iterations << std::endl;
          //sleep(0.5);
        }
        //sleep(0.1);
        H5FD_dsm_dump();
      }

      // Clean up for next step
      if (dsmManager->GetUpdatePiece() == 0) {
//        std::cout << "Calling UpdateSteeredObjects" << std::endl;
      }
//      dsmManager->UpdateSteeredObjects();
      if (dsmManager->GetUpdatePiece() == 0) {
        std::cout << "Calling NotificationFinalize" << std::endl;
      }
        //sleep(0.5);
      dsmManager->NotificationFinalize();
//      dsmManager->WaitForNotification();

//      dsmManager->SetSteeringCommand("play");

    }
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
