#include "H5FDdsmTest.h"
//
#include <cstdlib>
#include <string>
#include <cstdio>

//----------------------------------------------------------------------------

int main(int argc, char **argv)
{
  MPI_Comm       comm = MPI_COMM_WORLD;
  H5FDdsmFloat64 remoteMB, MBytes, bandwidth;
  H5FDdsmUInt64  numParticles, SendBytes, Bytes;
  H5FDdsmInt32   dataSizeMB = 0;
  H5FDdsmFloat64 totaltime;
  H5FDdsmConstString fullname = "dsm";
  H5FDdsmConstString dsm_env = getenv("H5FD_DSM_CONFIG_PATH");
  std::string hdffile;
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  senderInit(argc, argv, dsmManager, &comm, &dataSizeMB);

  if (dsm_env) {
    hdffile = std::string(dsm_env) + std::string("/hdf-output.h5");
    if (dsmManager->GetUpdatePiece() == 0) {
      printf("# HDF output goes to : %s\n", hdffile.c_str());
    }
  }

  remoteMB = dsmManager->GetDsmBuffer()->GetTotalLength() / (1024.0 * 1024.0);

  if (dataSizeMB && (dataSizeMB < remoteMB)) {
    remoteMB = dataSizeMB;
  }

  // When writing multiple datasets the metadata size is increased so must keep
  // more space in the DSM
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

  for (int type = 0; type < TYPES; type++) {

    if (dsmManager->GetUpdatePiece() == 0) {
      if (type == 0) printf("# Writing to DSM "); else printf("# Writing to Disk ");
      printf("%llu particles/proc (%d dataset(s) of dim %d) -- %lf MB\n",
          numParticles, NUM_DATASETS, DIM_DATASETS, MBytes);
      printf("%-*s%*s", 10, "# NumProcs", 20, "Bandwidth (MB/s)");
      if (dsmManager->GetDsmBuffer()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_CYCLIC ||
          dsmManager->GetDsmBuffer()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_RANDOM) {
        printf("%*s", 20, "Block Size (Bytes)");
      }
      printf("\n");
      fflush(stdout);
    }

    // Warming up
    for (int skip = 0; skip < SKIP; skip++) {
      if (type == 0) {
        TestParticleWrite(fullname, numParticles, DIM_DATASETS, NUM_DATASETS,
            dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(),
            comm, dsmManager, usingHDF);
      }
      else if (type == 1) {
        TestParticleWrite(hdffile.c_str(), numParticles, DIM_DATASETS, NUM_DATASETS,
            dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(),
            comm, NULL, usingHDF);
      }
    }
    // end of warm up
    for (int loop = 0; loop < LOOPS; loop++) {
      totaltime = 0;
      for (int avg = 0; avg < AVERAGE; avg++) {
        if (type == 0) {
          // We have configured everything manually using the DSM manager, so pass the buffer
          // into the read/write code so that we can use the dsm that we have setup
          // otherwise it creates a new DSM server object
          totaltime += TestParticleWrite(fullname, numParticles, DIM_DATASETS, NUM_DATASETS,
              dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(),
              comm, dsmManager, usingHDF);
        }
        else if (type == 1) {
          totaltime += TestParticleWrite(hdffile.c_str(), numParticles, DIM_DATASETS, NUM_DATASETS,
              dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(),
              comm, NULL, usingHDF);
        }
      }
      totaltime = totaltime / AVERAGE;
      bandwidth = (MBytes / totaltime);
      if (dsmManager->GetUpdatePiece() == 0) {
        printf("%-*d%*.*f", 10, dsmManager->GetUpdateNumPieces(), 20, 2, bandwidth);
        if (dsmManager->GetDsmBuffer()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_CYCLIC
            || dsmManager->GetDsmBuffer()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_RANDOM) {
          printf("%*lld", 20, dsmManager->GetDsmBuffer()->GetBlockLength());
        }
        printf("\n");
        fflush(stdout);
      }
    }
  }

  senderFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
