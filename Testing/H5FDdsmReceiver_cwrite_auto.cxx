#include "H5FDdsmTest.h"
#include "H5FDdsmTools.h"

#include <cstdlib>
#include <H5FDdsm_sys/SystemTools.hxx>

//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  MPI_Comm comm = MPI_COMM_WORLD;
  receiverInit(argc, argv, dsmManager, &comm);

  while (dsmManager->GetIsActive()) {
    std::cout << dsmManager->GetDsmBuffer()->GetComm()->GetId() << " Test Receiver waiting for unlock " << std::endl;
    if (dsmManager->WaitForUnlock() != H5FD_DSM_FAIL) {
      std::cout << dsmManager->GetDsmBuffer()->GetComm()->GetId() << " Unlocked : doing dump " << std::endl;
//      H5FDdsm_sys::SystemTools::Delay(5000);
      H5FD_dsm_dump();
//      H5FDdsm_sys::SystemTools::Delay(5000);
      // Sync here
      MPI_Barrier(comm);
      // Clean up for next step
//      dsmManager->NotificationFinalize();
//      H5FDdsm_sys::SystemTools::Delay(5000);
    }
    else {
      std::cout << dsmManager->GetDsmBuffer()->GetComm()->GetId() << " Wait for Unlock - disconnected " << std::endl;
    }
  }

  std::cout << dsmManager->GetDsmBuffer()->GetComm()->GetId() << " Disconnected " << std::endl;

  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
