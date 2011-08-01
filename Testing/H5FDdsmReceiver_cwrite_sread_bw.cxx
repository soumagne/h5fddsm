#include "H5FDdsmTest.h"

#include <cstdlib>

//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmInt32 nRemoteProcs;
  MPI_Comm comm = MPI_COMM_WORLD;
  H5FDdsmConstString fullname = "dsm";
  H5FDdsmUInt64 numParticles;
  H5FDdsmFloat64 remoteMB;
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  receiverInit(argc, argv, dsmManager, &comm);

  nRemoteProcs = dsmManager->GetDSMHandle()->GetComm()->GetInterSize();
  remoteMB = dsmManager->GetDSMHandle()->GetTotalLength() / (1024.0 * 1024.0);
  numParticles = (H5FDdsmUInt64) (1024 * 1024 * (remoteMB - 1) /
      (sizeof(H5FDdsmFloat64) * nRemoteProcs));

  while (dsmManager->GetDsmIsConnected()) {
    if (dsmManager->WaitForNotification() > 0) {
      // H5Dump
      // dsmManager->H5DumpLight();

      // Check data
      if (dsmManager->GetUpdatePiece() == 0) {
        // printf("Trying to read %d * %llu particles\n", nRemoteProcs, numParticles);
      }
      TestParticleRead(fullname, dsmManager->GetUpdatePiece(), nRemoteProcs * numParticles, comm, dsmManager->GetDSMHandle());
      // Sync here
      MPI_Barrier(comm);
      // Clean up for next step
      dsmManager->UpdateFinalize();
    }
  }

  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
