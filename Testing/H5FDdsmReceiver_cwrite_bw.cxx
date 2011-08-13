#include "H5FDdsmTest.h"

#include <cstdlib>

//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  MPI_Comm comm = MPI_COMM_WORLD;
  receiverInit(argc, argv, dsmManager, &comm);

  while (dsmManager->GetIsConnected()) {
    if (dsmManager->WaitForNotification() > 0) {
      // H5Dump
      // dsmManager->H5DumpLight();
      // Sync here
      MPI_Barrier(comm);
      // Clean up for next step
      dsmManager->NotificationFinalize();
    }
  }

  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
