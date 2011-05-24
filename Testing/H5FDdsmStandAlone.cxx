#include "H5FDdsmTest.h"

#include <cstdlib>

//----------------------------------------------------------------------------
int
main(int argc, char * argv[])
{
  MPI_Init(&argc, &argv);

  H5FDdsmInt32 rank, size;
  H5FDdsmUInt64 numParticles, dataMB;
  H5FDdsmConstString fullname = "dsm";

  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  // Create DSM Server
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  dsmManager->SetCommunicator(comm);
  dsmManager->SetLocalBufferSizeMBytes(16);
  dsmManager->CreateDSM();

  dataMB = dsmManager->GetDSMHandle()->GetTotalLength() / (1024.0 * 1024.0);
  numParticles = (H5FDdsmUInt64) ((1024 * 1024 * dataMB - 1) /
      (sizeof(H5FDdsmFloat64) * dsmManager->GetUpdateNumPieces()));

  // Write Data
  TestParticleWrite(fullname, numParticles, 1, 1, dsmManager->GetUpdatePiece(),
      dsmManager->GetUpdateNumPieces(), comm, dsmManager->GetDSMHandle(), usingHDF);

  // Read and Check Data
  TestParticleRead(fullname, dsmManager->GetUpdatePiece(),
      dsmManager->GetUpdateNumPieces() * numParticles, comm,
      dsmManager->GetDSMHandle());

  delete dsmManager;

  MPI_Finalize();

  return EXIT_SUCCESS;
}
