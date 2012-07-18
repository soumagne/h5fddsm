#include "H5FDdsmTest.h"
#include "H5FDdsmTools.h"

#include <cstdlib>

//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  MPI_Comm comm = MPI_COMM_WORLD;
  receiverInit(argc, argv, dsmManager, &comm);

  while (dsmManager->GetIsActive()) {
    if (dsmManager->WaitForUnlock() != H5FD_DSM_FAIL) {
      H5FD_dsm_dump();
      // Sync here
      MPI_Barrier(comm);
      // Clean up for next step
//      dsmManager->NotificationFinalize();
    }
  }

  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
