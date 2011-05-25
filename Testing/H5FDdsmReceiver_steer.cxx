#include "H5FDdsmTest.h"
#include "H5FDdsmSteering.h"

#include <cstdlib>

//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  MPI_Comm comm = MPI_COMM_WORLD;
  H5FDdsmInt32 step = 0;
  receiverInit(argc, argv, dsmManager, &comm);

  while (dsmManager->GetDsmIsConnected()) {
    if (dsmManager->WaitForUpdateReady() > 0) {
      H5FDdsmInt32 waitForGui = 1;
      H5FDdsmInt32 center[] = {1, 2, 3};
      // H5Dump
      // dsmManager->H5DumpLight();
      // Sync here
      MPI_Barrier(comm);
      switch (step) {
      case 0:
        // Position disabled
        dsmManager->SetDisabledObject("Position");
        break;
      case 1:
        // Position re-enabled
        dsmManager->SetDisabledObject("Position");
        break;
      case 2:
        // Simulate control taken by an arbitrary app, modifies values and
        // give back hand by sending a play
        dsmManager->SetSteeringValues("WaitForGui", 1, &waitForGui);
        dsmManager->SetSteeringValues("NewCenter", 3, center);
        dsmManager->SetSteeringCommand("play");
        break;
      default:
        break;
      }
      // Clean up for next step
      dsmManager->UpdateSteeredObjects();
      dsmManager->UpdateFinalize();
      step++;
    }
  }

  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
