#include "H5FDdsmTest.h"
#include "H5FDdsm.h"
//
#include <cstdlib>
#include <string>
#include <cstdio>

//----------------------------------------------------------------------------
#define LOOPS       1
#define AVERAGE     20
#define SKIP        10
#define TYPES       1 // 2 if disk output test required
//----------------------------------------------------------------------------

int main(int argc, char **argv)
{
  MPI_Comm       comm = MPI_COMM_WORLD;
  H5FDdsmFloat64 remoteMB, MBytes, Bytes, SendBytes, bandwidth;
  H5FDdsmUInt64  numParticles;
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
      std::cout << "# HDF output goes to : " << hdffile.c_str() << std::endl;
    }
  }

  if (dataSizeMB) {
    remoteMB = dataSizeMB;
  } else {
    remoteMB = dsmManager->GetDSMHandle()->GetTotalLength() / (1024.0 * 1024.0);
  }

  numParticles = (H5FDdsmUInt64) ((1024 * 1024 * remoteMB - H5FD_DSM_ALIGNMENT) /
      (sizeof(H5FDdsmFloat64) * 3.0 * dsmManager->GetUpdateNumPieces()));
  if (dsmManager->GetDSMHandle()->GetDsmType() == H5FD_DSM_TYPE_DYNAMIC_MASK) {
    dsmManager->GetDSMHandle()->SetMaskLength(numParticles * sizeof(H5FDdsmFloat64) * 3 * dsmManager->GetUpdateNumPieces());
    dsmManager->GetDSMHandle()->SendMaskLength();
  }
  Bytes       = numParticles * sizeof(H5FDdsmFloat64) * 3.0; // 3 = {x,y,z}
  SendBytes   = Bytes * dsmManager->GetUpdateNumPieces();
  MBytes      = SendBytes / (1024.0 * 1024.0);

  for (int type = 0; type < TYPES; type++) {
    if (type == 0 && dsmManager->GetUpdatePiece() == 0) {
      printf("# Writing to DSM ");
    }
    else if (type == 1 && dsmManager->GetUpdatePiece() == 0) {
      printf("# Writing to Disk ");
    }
    if (dsmManager->GetUpdatePiece() == 0) {
      printf("%ld particles/proc (3 arrays) -- %lf MB\n", numParticles, MBytes);
      printf("%-*s%*s", 10, "# NumProcs", 20, "Bandwidth (MB/s)");
      if (dsmManager->GetDSMHandle()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_CYCLIC ||
          dsmManager->GetDSMHandle()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_RANDOM) {
        printf("%*s", 20, "Block Size (Bytes)");
      }
      printf("\n");
      fflush(stdout);
    }
    if (MBytes < remoteMB) {
      // Warming up
      for (int skip = 0; skip < SKIP; skip++) {
        if (type == 0) {
          TestParticleWrite(fullname, numParticles, 3, dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(), comm, dsmManager->GetDSMHandle(), usingHDF);
        }
        else if (type == 1) {
          TestParticleWrite(hdffile.c_str(), numParticles, 3, dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(), comm, NULL, usingHDF);
        }
      }
      for (int loop = 0; loop < LOOPS; loop++) {
        totaltime = 0;
        for (int avg = 0; avg < AVERAGE; avg++) {
          if (type == 0) {
            // We have configured everything manually using the DSM manager, so pass the buffer
            // into the read/write code so that we can use the dsm that we have setup
            // otherwise it creates a new DSM server object
            totaltime += TestParticleWrite(fullname, numParticles, 3, dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(), comm, dsmManager->GetDSMHandle(), usingHDF);
          }
          else if (type == 1) {
            totaltime += TestParticleWrite(hdffile.c_str(), numParticles, 3, dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(), comm, NULL, usingHDF);
          }
        }
        totaltime = totaltime / AVERAGE;
        bandwidth = (MBytes / totaltime);
        if (dsmManager->GetUpdatePiece() == 0) {
          printf("%-*d%*.*f", 10, dsmManager->GetUpdateNumPieces(), 20, 2, bandwidth);
          if (dsmManager->GetDSMHandle()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_CYCLIC
              || dsmManager->GetDSMHandle()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_RANDOM) {
            printf("%*ld", 20, dsmManager->GetDSMHandle()->GetBlockLength());
          }
          printf("\n");
          fflush(stdout);
        }
      }
    }
  }

  senderFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
