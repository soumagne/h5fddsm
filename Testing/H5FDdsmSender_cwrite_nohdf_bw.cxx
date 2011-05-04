#include "H5FDdsmTest.h"
//
#include <cstdlib>
#include <string>

typedef struct EcritParticuleHDFTesting {
  H5FDdsmFloat64 *Ddata;
} EcritParticuleHDFTesting;

/*******************************************************************/
/* len   = number this process will write                          */
/* start = start index for this write                              */
/* total = total to be written by all all processes                */
/*******************************************************************/
void write_ecrit_particule(
    EcritParticuleHDFTesting *buf, H5FDdsmConstString filename,
    H5FDdsmUInt64 len,  H5FDdsmUInt64 start, H5FDdsmUInt64 total, H5FDdsmBuffer *dsmBuffer)
{
  H5FDdsmAddr metadataAddr = (H5FDdsmAddr) (dsmBuffer->GetTotalLength() - sizeof(H5FDdsmMetaData));
  H5FDdsmEntry entry;
  H5FDdsmBoolean dirty = 0, isSomeoneDirty = 0;

  // Simulate open
  if (!dsmBuffer->GetIsLocked() && dsmBuffer->GetIsConnected()) {
    dsmBuffer->RequestLockAcquire();
  }
  // Simulate get of DSM metadata
  if (dsmBuffer->Get(metadataAddr, sizeof(entry), &entry) != H5FD_DSM_SUCCESS) {
    std::cerr << "DsmGetEntry failed" << std::endl;
    return;
  }
  dsmBuffer->GetComm()->Barrier();


  // Simulate write
  if (dsmBuffer->Put(start*sizeof(H5FDdsmFloat64), len*sizeof(H5FDdsmFloat64), (void *) (*buf).Ddata) != H5FD_DSM_SUCCESS) {
    std::cerr << "can't write to DSM" << std::endl;
    return;
  }
  dirty = 1;

  // Simulate close
  if (dsmBuffer->GetComm()->GetId() == 0) {
    if (dsmBuffer->Put(metadataAddr, sizeof(entry), &entry) != H5FD_DSM_SUCCESS) {
      std::cerr << "DsmUpdateEntry failed" << std::endl;
      return;
    }
  }
  dsmBuffer->GetComm()->Barrier();
  MPI_Allreduce(&dirty, &isSomeoneDirty, sizeof(H5FDdsmBoolean),
      MPI_UNSIGNED_CHAR, MPI_MAX, dsmBuffer->GetComm()->GetComm());
  dsmBuffer->SetIsDataModified(1);
  dsmBuffer->RequestServerUpdate();

}
//----------------------------------------------------------------------------
// Test Particle write (H5Part compatible)
//----------------------------------------------------------------------------
void initBuffer(EcritParticuleHDFTesting *buffer)
{
  (*buffer).Ddata = NULL;
}

//----------------------------------------------------------------------------
void freeBuffer(EcritParticuleHDFTesting *buffer)
{
  if ((*buffer).Ddata) free((*buffer).Ddata);
}

//----------------------------------------------------------------------------
H5FDdsmFloat64 TestParticleWrite(H5FDdsmConstString filename, H5FDdsmUInt64 N, H5FDdsmInt32 mpiId, H5FDdsmInt32 mpiNum,
    MPI_Comm dcomm, H5FDdsmBuffer *dsmBuffer)
{
  EcritParticuleHDFTesting WriteBuffer;
  H5FDdsmUInt64 i, start, total;
  H5FDdsmFloat64 *doublearray;

  start = N*mpiId;
  total = N*mpiNum;
  // set all array pointers to zero
  initBuffer(&WriteBuffer);

  // create arrays for the test vars we selected above
  doublearray = (H5FDdsmFloat64*) malloc(sizeof(H5FDdsmFloat64)*N);//(H5FDdsmFloat64*) malloc(sizeof(H5FDdsmFloat64)*3*N);
  for (i=0; i<N; i++) {
    doublearray[i] = 0.1*i;
//    doublearray[3*i]   = 0.1*i;
//    doublearray[3*i+1] = 0.1*i;
//    doublearray[3*i+2] = 0.1*i;
  }
  WriteBuffer.Ddata = doublearray;

  // call the write routine with our dummy buffer
  MPI_Barrier(dcomm);
//  fprintf(stderr, "(%d) writing %ld particles from %ld\n", dsmBuffer->GetComm()->GetId(), N, start);
  H5FDdsmFloat64 t1 = MPI_Wtime();
  write_ecrit_particule(&WriteBuffer, filename, N, start, total, dsmBuffer);
  MPI_Barrier(dcomm);
  H5FDdsmFloat64 t2 = MPI_Wtime();

  // free all array pointers
  freeBuffer(&WriteBuffer);
  return t2-t1;
};

//----------------------------------------------------------------------------
#define LOOPS       10
#define AVERAGE     5
#define TYPES       1 // 2 if disk output test required

int main(int argc, char **argv)
{
  MPI_Comm       comm = MPI_COMM_WORLD;
  H5FDdsmFloat64 remoteMB, MBytes, Bytes, SendBytes, bandwidth;
  H5FDdsmInt32 dataSizeMB = 0;
  H5FDdsmFloat64 totaltime;
  H5FDdsmConstString fullname = "dsm";
  H5FDdsmConstString dsm_env = getenv("H5FD_DSM_CONFIG_PATH");
  std::string hdffile;
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  senderInit(argc, argv, dsmManager, &comm, &dataSizeMB);

  if (dsm_env) {
    hdffile = std::string(dsm_env) + std::string("/hdf-output.h5");
    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "HDF output goes to : " << hdffile.c_str() << std::endl;
    }
  }

  if (dataSizeMB) {
    remoteMB = dataSizeMB;
  } else {
    remoteMB = dsmManager->GetDSMHandle()->GetTotalLength() / (1024.0 * 1024.0);
  }

  for (int type = 0; type < TYPES; type++) {
    if (type == 0 && dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Writing to DSM" << std::endl;
    }
    else if (type == 1 && dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Writing to Disk" << std::endl;
    }
    for (int loop = 0; loop < LOOPS; loop++) {
      H5FDdsmUInt64 numParticles = (H5FDdsmUInt64) ((1024 * 1024 * remoteMB - H5FD_DSM_ALIGNMENT) /
          (sizeof(H5FDdsmFloat64) * 1.0 * dsmManager->GetUpdateNumPieces())); // 3 = {x,y,z}
      if (dsmManager->GetDSMHandle()->GetDsmType() == H5FD_DSM_TYPE_DYNAMIC_MASK) {
        dsmManager->GetDSMHandle()->SetMaskLength(numParticles * sizeof(H5FDdsmFloat64) * dsmManager->GetUpdateNumPieces());
        dsmManager->GetDSMHandle()->SendMaskLength();
      }
      Bytes       = numParticles * sizeof(H5FDdsmFloat64) * 1.0;
      SendBytes   = Bytes * dsmManager->GetUpdateNumPieces();
      MBytes      = SendBytes / (1024.0 * 1024.0);
      if (MBytes < remoteMB) {
        totaltime = 0;
        for (int avg = 0; avg < AVERAGE; avg++) {
          if (type == 0) {
            // We have configured everything manually using the DSM manager, so pass the buffer
            // into the read/write code so that we can use the dsm that we have setup
            // otherwise it creates a new DSM server object
            totaltime += TestParticleWrite(fullname, numParticles, dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(), comm, dsmManager->GetDSMHandle());
          }
          else if (type == 1) {
            totaltime += TestParticleWrite(hdffile.c_str(), numParticles, dsmManager->GetUpdatePiece(), dsmManager->GetUpdateNumPieces(), comm, NULL);
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
