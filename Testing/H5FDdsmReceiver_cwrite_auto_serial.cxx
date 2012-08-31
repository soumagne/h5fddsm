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
    }
  }

  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
