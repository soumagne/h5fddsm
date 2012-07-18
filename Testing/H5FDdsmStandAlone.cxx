#include "H5FDdsmTest.h"
#include "H5FDdsm.h"

#include <cstdlib>

//----------------------------------------------------------------------------
int
main(int argc, char * argv[])
{
  H5FDdsmInt32 provided, rank, size;
  H5FDdsmUInt32 dsmSize, numServers; // default MB
  H5FDdsmUInt64 numParticles;
  H5FDdsmFloat64 dataMB;
  H5FDdsmConstString fullname = "dsm";
  MPI_Comm comm = MPI_COMM_WORLD;
  H5FDdsmInt32 exit_status = EXIT_SUCCESS;

  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  dsmSize = size * 4;
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
  dsmManager->SetIsStandAlone(H5FD_DSM_TRUE);
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

  numParticles = (H5FDdsmUInt64) ((1024 * 1024 * dataMB - H5FD_DSM_ALIGNMENT * NUM_DATASETS) /
      (sizeof(H5FDdsmFloat64) * DIM_DATASETS * NUM_DATASETS * dsmManager->GetUpdateNumPieces()));

  H5FD_dsm_set_options(H5FD_DSM_LOCK_ASYNCHRONOUS);

  std::cout << "Writing Standalone " << std::endl;
  // Write Data
  TestParticleWrite(fullname, numParticles, DIM_DATASETS, NUM_DATASETS,
      dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(), comm,
      dsmManager, usingHDF);

  std::cout << "Reading Standalone " << std::endl;
  // Read and Check Data
  if (TestParticleRead(fullname, numParticles, DIM_DATASETS, NUM_DATASETS,
      dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(), comm,
      dsmManager) == H5FD_DSM_FAIL)
    exit_status = EXIT_FAILURE;

  H5close();
  std::cout << "Finishing Standalone " << std::endl;
  delete dsmManager;

  std::cout << "Finalizing Standalone " << std::endl;
  MPI_Finalize();

  std::cout << "Exiting Standalone " << std::endl;
  return(exit_status);
}
