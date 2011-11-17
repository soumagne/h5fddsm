#include "H5FDdsmTest.h"
#include "H5FDdsm.h"

#include <cstdlib>

//----------------------------------------------------------------------------
int
main(int argc, char * argv[])
{
  H5FDdsmInt32 provided, rank, size;
  H5FDdsmUInt32 dsmSize = 16, numServers; // default MB
  H5FDdsmUInt64 numParticles;
  H5FDdsmFloat64 dataMB;
  H5FDdsmConstString fullname = "dsm";
  MPI_Comm comm = MPI_COMM_WORLD;

  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  //
  if (rank == 0) {
    if (provided != MPI_THREAD_MULTIPLE) {
      std::cout << "# MPI_THREAD_MULTIPLE not set, you may need to recompile your "
          << "MPI distribution with threads enabled" << std::endl;
    }
    else {
      std::cout << "# MPI_THREAD_MULTIPLE is OK" << std::endl;
    }
  }

  // Create DSM Server
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  dsmManager->SetMpiComm(comm);
  dsmManager->SetLocalBufferSizeMBytes(dsmSize / size);
  dsmManager->Create();
  H5FD_dsm_set_manager(dsmManager);

  dataMB = dsmManager->GetDsmBuffer()->GetTotalLength() / (1024.0 * 1024.0);
  numServers = dsmManager->GetDsmBuffer()->GetEndServerId() -
      dsmManager->GetDsmBuffer()->GetStartServerId() + 1;
  if (rank == 0) {
    std::cout << "# DSM server memory size is: " << dataMB << " MBytes"
        << " (" << dsmManager->GetDsmBuffer()->GetTotalLength() << " Bytes)" << std::endl;
    std::cout << "# DSM server process count: " <<  numServers << std::endl;
  }

  numParticles = (H5FDdsmUInt64) ((1024 * 1024 * dataMB - H5FD_DSM_ALIGNMENT) /
      (sizeof(H5FDdsmFloat64) * dsmManager->GetUpdateNumPieces()));

  // Write Data
  TestParticleWrite(fullname, numParticles, 1, 1, dsmManager->GetUpdatePiece(),
      dsmManager->GetUpdateNumPieces(), comm, dsmManager, usingHDF);

  // Read and Check Data
  TestParticleRead(fullname, dsmManager->GetUpdatePiece(),
      dsmManager->GetUpdateNumPieces() * numParticles, comm, dsmManager);

  delete dsmManager;

  MPI_Finalize();

  return EXIT_SUCCESS;
}
