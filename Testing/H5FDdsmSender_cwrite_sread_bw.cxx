#include "H5FDdsmTest.h"
#include "H5FDdsm.h"
//
#include <hdf5.h>
#include <cstdlib>
#include <string>

//----------------------------------------------------------------------------
#define LOOPS       1
#define AVERAGE     5
#define TYPES       1 // 2 if disk output test required
//----------------------------------------------------------------------------

int main(int argc, char **argv)
{
  MPI_Comm       comm = MPI_COMM_WORLD;
  H5FDdsmFloat64 remoteMB, MBytes, Bytes, SendBytes, bandwidth;
  H5FDdsmFloat64 totaltime;
  H5FDdsmConstString fullname = "dsm";
  H5FDdsmConstString dsm_env = getenv("H5FD_DSM_CONFIG_PATH");
  std::string hdffile;
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  senderInit(argc, argv, dsmManager, &comm);

  if (dsm_env) {
    hdffile = std::string(dsm_env) + std::string("/hdf-output.h5");
    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "# HDF output goes to : " << hdffile.c_str() << std::endl;
    }
  }

  remoteMB = dsmManager->GetDSMHandle()->GetTotalLength() / (1024.0 * 1024.0);

  for (int type = 0; type < TYPES; type++) {
    if (type == 0 && dsmManager->GetUpdatePiece() == 0) {
      std::cout << "# Writing to DSM" << std::endl;
    }
    else if (type == 1 && dsmManager->GetUpdatePiece() == 0) {
      std::cout << "# Writing to Disk" << std::endl;
    }
    for (int loop = 0; loop < LOOPS; loop++) {
      H5FDdsmUInt64 numParticles = (H5FDdsmUInt64) (1024 * 1024 * (remoteMB - 1) /
          (sizeof(H5FDdsmFloat64) * dsmManager->GetUpdateNumPieces()));
      Bytes       = (H5FDdsmFloat64) (numParticles * sizeof(H5FDdsmFloat64));
      SendBytes   = Bytes * dsmManager->GetUpdateNumPieces();
      MBytes      = SendBytes / (1024.0 * 1024.0);
      if (MBytes < remoteMB) {
        totaltime = 0;
        for (int avg = 0; avg < AVERAGE; avg++) {
          if (type == 0) {
            // We have configured everything manually using the DSM manager, so pass the buffer
            // into the read/write code so that we can use the dsm that we have setup
            // otherwise it creates a new DSM server object
            totaltime += TestParticleWrite(fullname, numParticles, 1, dsmManager->GetUpdatePiece(),
                dsmManager->GetUpdateNumPieces(), comm, dsmManager->GetDSMHandle(), usingHDF);
          }
          else if (type==1) {
            totaltime += TestParticleWrite(hdffile.c_str(), numParticles, 1, dsmManager->GetUpdatePiece(),
                dsmManager->GetUpdateNumPieces(), comm, NULL, usingHDF);
          }
        }
        totaltime = totaltime / AVERAGE;
        bandwidth = (MBytes / totaltime);
        if (dsmManager->GetUpdatePiece() == 0) {
          std::cout << "Particles/proc, "   << numParticles << ", "
                    << "NumArrays, "        << 1            << ", "
                    << "NProcs, "           << dsmManager->GetUpdateNumPieces()  << ", "
                    << "Mbytes, "           << MBytes       << ", "
                    << "TotalTime, "        << totaltime    << ", "
                    << "Bandwidth (MB/s), " << bandwidth    << std::endl;
        }
      }
    }
  }

  senderFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
