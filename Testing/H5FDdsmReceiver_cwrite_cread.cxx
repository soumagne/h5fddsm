#include "H5FDdsmTest.h"
#include "H5FDdsmTools.h"

#include <cstdlib>

//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  MPI_Comm comm = MPI_COMM_WORLD;
  receiverInit(argc, argv, dsmManager, &comm);

  do {
    std::cout << "dsmManager->WaitForUnlock() " << std::endl;
    if (dsmManager->WaitForUnlock() != H5FD_DSM_FAIL) {
      std::cout << "start dsmManager->GetIsConnected() " << dsmManager->GetIsConnected() << std::endl;
      H5FD_dsm_dump();
      std::cout << "end   dsmManager->GetIsConnected() " << dsmManager->GetIsConnected() << std::endl;
      // Sync here
      MPI_Barrier(comm);
      // Clean up for next step
//      dsmManager->NotificationFinalize();
    }
  } while (dsmManager->GetIsActive());

  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
