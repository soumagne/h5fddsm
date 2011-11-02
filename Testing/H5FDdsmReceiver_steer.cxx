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

  while (dsmManager->GetIsConnected()) {
    if (dsmManager->WaitForNotification() > 0) {
      H5FDdsmInt32 intScalar = 1;
      H5FDdsmInt32 intVector[] = {1, 2, 3};
      H5FDdsmFloat64 doubleScalar = 3.14;
      H5FDdsmFloat64 doubleVector[] = {1.001, 2.001, 3.001};
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
        dsmManager->SetSteeringValues("IntScalarTest", 1, &intScalar);
        dsmManager->SetSteeringValues("IntVectorTest", 3, intVector);
        dsmManager->SetSteeringValues("DoubleScalarTest", 1, &doubleScalar);
        dsmManager->SetSteeringValues("DoubleVectorTest", 3, doubleVector);
//        dsmManager->SetSteeringCommand("play");
        break;
      case 3:
        dsmManager->H5DumpLight();
      default:
        break;
      }
      // Clean up for next step
      dsmManager->UpdateSteeredObjects();
      dsmManager->NotificationFinalize();
      step++;
    }
  }

  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
