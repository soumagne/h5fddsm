#include "H5FDdsmTest.h"
#include "H5FDdsm.h"
#include "H5FDdsmTools.h"

#include <cstdlib>

//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmInt32 exit_status = EXIT_SUCCESS;
  H5FDdsmInt32 nRemoteProcs;
  MPI_Comm comm = MPI_COMM_WORLD;
  H5FDdsmConstString fullname = "dsm";
  H5FDdsmFloat64 remoteMB, MBytes, bandwidth;
  H5FDdsmUInt64  numParticles, RecvBytes, Bytes, numRemoteParticles;
  H5FDdsmFloat64 totaltime;
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  receiverInit(argc, argv, dsmManager, &comm);

  nRemoteProcs = dsmManager->GetDsmBuffer()->GetComm()->GetInterSize();
  remoteMB = dsmManager->GetDsmBuffer()->GetTotalLength() / (1024.0 * 1024.0);
  numRemoteParticles = (H5FDdsmUInt64) ((1024 * 1024 * remoteMB - H5FD_DSM_ALIGNMENT * NUM_DATASETS) /
      (sizeof(H5FDdsmFloat64) * DIM_DATASETS * NUM_DATASETS * nRemoteProcs));

  numParticles = numRemoteParticles * nRemoteProcs / dsmManager->GetUpdateNumPieces();
  Bytes       = numParticles * sizeof(H5FDdsmFloat64) * DIM_DATASETS * NUM_DATASETS; // 3 = {x,y,z}
  RecvBytes   = Bytes * dsmManager->GetUpdateNumPieces();
  MBytes      = RecvBytes / (1024.0 * 1024.0);

  if (numRemoteParticles * nRemoteProcs - numParticles * dsmManager->GetUpdateNumPieces() < 0) {
    if (dsmManager->GetUpdatePiece() == 0) fprintf(stderr, "Cannot receive, too many tuples/proc!\n");
    fflush(stderr);
    receiverFinalize(dsmManager, &comm);
    delete dsmManager;
    return(EXIT_SUCCESS);
  }

  while (dsmManager->GetIsConnected()) {

    if (dsmManager->GetUpdatePiece() == 0) {
      printf("# Receiving from DSM ");
      printf("%lu particles/proc (%d x %d) -- %lf MB\n",
          numParticles, NUM_DATASETS, DIM_DATASETS, MBytes);
      printf("%-*s%*s", 10, "# NumProcs", 20, "Bandwidth (MB/s)");
      if (dsmManager->GetDsmBuffer()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_CYCLIC ||
          dsmManager->GetDsmBuffer()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_RANDOM) {
        printf("%*s", 20, "Block Size (Bytes)");
      }
      printf("\n");
      fflush(stdout);
    }

    for (int loop = 0; loop < LOOPS; loop++) {
      totaltime = 0;
      for (int avg = 0; avg < AVERAGE; avg++) {
        if (dsmManager->WaitForNotification() > 0) {
          H5FDdsmFloat64 readtime;
          // H5FD_dsm_dump();
          readtime = TestParticleRead(fullname, numParticles, DIM_DATASETS, NUM_DATASETS,
              dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(),
              comm, dsmManager);
          if (readtime != H5FD_DSM_FAIL) {
            totaltime += readtime;
          } else {
            exit_status = EXIT_FAILURE;
          }
          // Sync here
          MPI_Barrier(comm);
          // Clean up for next step
          dsmManager->NotificationFinalize();
        }
      }
      totaltime = totaltime / AVERAGE;
      bandwidth = (MBytes / totaltime);
      if (dsmManager->GetUpdatePiece() == 0) {
        printf("%-*d%*.*f", 10, dsmManager->GetUpdateNumPieces(), 20, 2, bandwidth);
        if (dsmManager->GetDsmBuffer()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_CYCLIC
            || dsmManager->GetDsmBuffer()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_RANDOM) {
          printf("%*ld", 20, dsmManager->GetDsmBuffer()->GetBlockLength());
        }
        printf("\n");
        fflush(stdout);
      }
    }
    dsmManager->WaitForNotification();
  }

  receiverFinalize(dsmManager, &comm);
  delete dsmManager;
  return(exit_status);
}
