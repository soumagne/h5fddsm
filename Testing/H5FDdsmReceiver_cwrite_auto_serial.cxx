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

  // -------------------------------------------
  // Serial mode test
  // processes read independently, but before doing so, 
  // we must override auto unlock of the DSM by calling
  //   H5FD_dsm_set_options(H5FD_DSM_UNLOCK_MANUAL);
  // and collectively lock the dsm before use by calling
  //   H5FD_dsm_lock();
  // and collectively unlock the file after use by calling 
  //   H5FD_dsm_unlock();
  // -------------------------------------------

//  H5FD_dsm_set_options(H5FD_DSM_UNLOCK_MANUAL);
  while (dsmManager->GetIsActive()) {
    if (dsmManager->WaitForUnlock() != H5FD_DSM_FAIL) {
      H5FD_dsm_lock();
      
      H5FD_dsm_set_options(H5FD_DSM_MODE_SERIAL);
      if (dsmManager->GetUpdatePiece() == 0) {
        H5FD_dsm_dump();
      }
      H5FD_dsm_set_options(H5FD_DSM_MODE_PARALLEL);

      // since we did nothing, there's no need to signal new data or anything else
      H5FD_dsm_unlock(H5FD_DSM_NOTIFY_NONE);
      // Sync here
    }
  }
  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
