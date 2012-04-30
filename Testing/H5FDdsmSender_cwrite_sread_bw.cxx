#include "H5FDdsmTest.h"
#include "H5FDdsm.h"
//
#include <hdf5.h>
#include <cstdlib>
#include <string>

//----------------------------------------------------------------------------

int main(int argc, char **argv)
{
  MPI_Comm       comm = MPI_COMM_WORLD;
  H5FDdsmFloat64 remoteMB, MBytes;
  H5FDdsmUInt64  numParticles, SendBytes, Bytes;
  H5FDdsmConstString fullname = "dsm";
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  senderInit(argc, argv, dsmManager, &comm);

  remoteMB = dsmManager->GetDsmBuffer()->GetTotalLength() / (1024.0 * 1024.0);

  // When writing multiple datasets the metadata size is increased so must
  // keep more space in the DSM
  numParticles = (H5FDdsmUInt64) ((1024 * 1024 * remoteMB - H5FD_DSM_ALIGNMENT * NUM_DATASETS) /
      (sizeof(H5FDdsmFloat64) * DIM_DATASETS * NUM_DATASETS * dsmManager->GetUpdateNumPieces()));
  Bytes       = numParticles * sizeof(H5FDdsmFloat64) * DIM_DATASETS * NUM_DATASETS; // 3 = {x,y,z}
  SendBytes   = Bytes * dsmManager->GetUpdateNumPieces();
  MBytes      = SendBytes / (1024.0 * 1024.0);

  if (MBytes > remoteMB) {
    if (dsmManager->GetUpdatePiece() == 0) fprintf(stderr, "Cannot write, remote DSM too small!\n");
    fflush(stderr);
    senderFinalize(dsmManager, &comm);
    delete dsmManager;
    return(EXIT_SUCCESS);
  }

  for (int loop = 0; loop < LOOPS; loop++) {
    for (int avg = 0; avg < AVERAGE; avg++) {
        // We have configured everything manually using the DSM manager, so pass the buffer
        // into the read/write code so that we can use the dsm that we have setup
        // otherwise it creates a new DSM server object
        TestParticleWrite(fullname, numParticles, DIM_DATASETS, NUM_DATASETS,
            dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(),
            comm, dsmManager, usingHDF);
    }
  }

  senderFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
