#include "H5FDdsmTest.h"
#include "H5FDdsm.h"
//
#include <hdf5.h>
#include <cstdlib>

// Names of datasets and groups
#define GROUPNAME       "Scalar"
#define DATASETNAME     "Pressure"

// data structures used when writing
typedef struct ParticleBuffer {
  double *Ddata;
} ParticleBuffer_t;

#define H5CHECK_ERROR(var, msg) if (var<0) printf("Error %s", msg);

//----------------------------------------------------------------------------
void particle_read(
    ParticleBuffer_t *buf, const char *filename, int rank,
    H5FDdsmBuffer *dsmBuffer)
{
  hid_t      file_id, group_id, dataset_id;
  hid_t      acc_plist_id;
  herr_t     status = 0;

  // Set up file access property list with parallel I/O
  acc_plist_id = H5Pcreate(H5P_FILE_ACCESS);
  H5CHECK_ERROR(acc_plist_id, "H5Pcreate(H5P_FILE_ACCESS)");
  if (dsmBuffer) {
    H5Pset_fapl_dsm(acc_plist_id, MPI_COMM_WORLD, dsmBuffer);
    H5CHECK_ERROR(status, "H5Pset_fapl_dsm");
  }

  // Create a new file collectively (overwrite existing)
  file_id = H5Fopen(filename, H5F_ACC_RDONLY, acc_plist_id);
  H5CHECK_ERROR(file_id, "H5Fopen");

  // Close property list
  status = H5Pclose(acc_plist_id);
  H5CHECK_ERROR(status, "H5Pclose(acc_plist_id)");

  // Open the group
  group_id = H5Gopen(file_id, GROUPNAME, H5P_DEFAULT);
  H5CHECK_ERROR(group_id, "H5Gopen");

  // Open the dataset
  dataset_id = H5Dopen(group_id, DATASETNAME, H5P_DEFAULT);
  H5CHECK_ERROR(dataset_id, "H5Dopen");

  // Read the dataset
  if (rank == 0) {
    status = H5Dread(dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, (*buf).Ddata);
    H5CHECK_ERROR(dataset_id, "H5Dread");
  }

  // Release resources
  status = H5Dclose(dataset_id);
  H5CHECK_ERROR(status, "H5Dclose");
  status = H5Gclose(group_id);
  H5CHECK_ERROR(status, "H5Gclose");
  status = H5Fclose(file_id);
  H5CHECK_ERROR(status, "H5Fclose");
}
//----------------------------------------------------------------------------
void initBuffer(ParticleBuffer_t *buffer) {
  (*buffer).Ddata = NULL;
}
//----------------------------------------------------------------------------
void freeBuffer(ParticleBuffer_t *buffer) {
  if ((*buffer).Ddata) free((*buffer).Ddata);
}
//----------------------------------------------------------------------------
double TestParticleRead(const char *filename, int rank, hsize_t N,
    MPI_Comm comm, H5FDdsmBuffer *dsmBuffer)
{
  H5FDdsmInt32 status = H5FD_DSM_SUCCESS;
  ParticleBuffer_t ReadBuffer;
  hsize_t   i;
  double   *doublearray;
  int fail_count = 0;
  static int step_increment = 0;

  // set all array pointers to zero
  initBuffer(&ReadBuffer);

  // create arrays for the test vars we selected above
  doublearray = (double*)malloc(sizeof(double)*N);
  for (i=0; i<N; i++) {
    doublearray[i] = 0;
  }
  ReadBuffer.Ddata = doublearray;

  // call the write routine with our dummy buffer
  MPI_Barrier(comm);
  particle_read(&ReadBuffer, filename, rank, dsmBuffer);

  if (rank == 0 && step_increment < 5) {
    /* Check the results the first times. */
    for (i=0; i<N; i++) {
      if((doublearray[i] != (i + step_increment)) && (fail_count<10)) {
        fprintf(stderr," doublearray[%llu] is %llu, should be %llu\n", i, (hsize_t)doublearray[i], (i+step_increment));
        fail_count++;
      }
    }
    if (fail_count == 0) {
        // if (rank == 0) printf("DSM read test PASSED\n");
    }
    else {
      fprintf(stderr,"DSM read test FAILED for PE %d, %d or more wrong values at step %d\n", rank, fail_count, step_increment);
    }
  }
  MPI_Bcast(&fail_count, 1, MPI_INT, 0, comm);
  if (fail_count > 0) status = H5FD_DSM_FAIL;
  // free all array pointers
  freeBuffer(&ReadBuffer);
  step_increment++;
  return(status);
};
//----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
  H5FDdsmInt32 nremoteprocs;
  MPI_Comm comm = MPI_COMM_WORLD;
  H5FDdsmConstString fullname = "dsm";
  hsize_t numParticles = 0;
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  receiverInit(argc, argv, dsmManager, &comm);

  while (dsmManager->GetDsmIsConnected()) {
    if (dsmManager->WaitForUpdateReady() > 0) {
      // H5Dump
      // dsmManager->H5DumpLight();
      nremoteprocs = dsmManager->GetDSMHandle()->GetComm()->GetInterSize();
      numParticles = (hsize_t) (1024*1024*((dsmManager->GetDSMHandle()->GetTotalLength()/(1024.0*1024.0))-1)/(sizeof(double)*nremoteprocs));
      // Check data
      if (dsmManager->GetUpdatePiece() == 0) {
        // printf("Trying to read %d * %llu particles\n", nremoteprocs, numParticles);
      }
      TestParticleRead(fullname, dsmManager->GetUpdatePiece(), nremoteprocs*numParticles, comm, dsmManager->GetDSMHandle());
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
