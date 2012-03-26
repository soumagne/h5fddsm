#include "H5FDdsmTest.h"
#include "H5FDdsm.h"
#include "H5FDdsmTools.h"

#include <cstdlib>

//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  MPI_Comm comm = MPI_COMM_WORLD;
  int rank;
  receiverInit(argc, argv, dsmManager, &comm);
  MPI_Comm_rank(comm, &rank);

  while (dsmManager->GetIsConnected()) {
    if (dsmManager->WaitForNotification() > 0) {
      // H5Dump
      if (rank == 0) {
        H5FD_dsm_set_options(H5FD_DSM_MODE_SERIAL);
        H5FD_dsm_dump();
      }
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
