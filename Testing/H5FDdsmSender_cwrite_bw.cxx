#include "H5FDdsmTest.h"
#include "H5FDdsm.h"
//
#include <hdf5.h>
#include <cstdlib>
#include <string>

/* Name of dataset_id to create in loc_id */
#define GROUPNAME       "Step#0"
#define TIME_ATTRIBUTE  "TimeValue"

/*******************************************************************/
/* data structures used when writing                               */
/*******************************************************************/
// used for comparing H5Part with Compound H5SPH
typedef struct EcritParticuleHDFTesting {
  H5FDdsmFloat64 *Ddata;
} EcritParticuleHDFTesting;

/* names of above arrays for convenience when using H5Part IO */
H5FDdsmConstString ArrayNames = "Position";

#define H5CHECK_ERROR(var, msg) if (var<0) printf("Error %s", msg);

/*******************************************************************/
/* len   = number this process will write                          */
/* start = start index for this write                              */
/* total = total to be written by all all processes                */
/* collective : 1=collective IO, 0=independent (default 0)         */
/*******************************************************************/
void write_ecrit_particule(
    EcritParticuleHDFTesting *buf, H5FDdsmConstString filename,
    H5FDdsmUInt64 len,  H5FDdsmUInt64 start, H5FDdsmUInt64 total, H5FDdsmBuffer *dsmBuffer)
{
  hid_t      file_id, group_id, dataset_id, xfer_plist_id;
  hid_t      file_space_id, mem_space_id;
  hid_t      acc_plist_id;
  hsize_t    count[2]    = {total, 3};
  hsize_t    offset[2]   = {start, 0};
  hsize_t    localdim[2] = {len, 3};
  herr_t     status = 0;

  /* Set up file access property list with parallel I/O */
  acc_plist_id = H5Pcreate(H5P_FILE_ACCESS);
  H5CHECK_ERROR(acc_plist_id, "H5Pcreate(H5P_FILE_ACCESS)");
  if (dsmBuffer) {
    H5FD_dsm_init();
    H5Pset_fapl_dsm(acc_plist_id, MPI_COMM_WORLD, dsmBuffer);
    H5CHECK_ERROR(status, "H5Pset_fapl_dsm");
  } else {
    H5Pset_fapl_mpio(acc_plist_id, MPI_COMM_WORLD, MPI_INFO_NULL);
    H5CHECK_ERROR(status, "H5Pset_fapl_mpio");
  }

  /* Create a new file collectively (overwrite existing) */
  file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_plist_id);
  H5CHECK_ERROR(file_id, "H5Fcreate");

  /* Close property list */
  status = H5Pclose(acc_plist_id);
  H5CHECK_ERROR(status, "H5Pclose(acc_plist_id)");

  /* Create the file_space_id */
  file_space_id = H5Screate_simple(2, count, NULL);
  H5CHECK_ERROR(file_space_id, "H5Screate_simple");

  // Create a group to hold this time step (for H5Part compatibility)
  group_id = H5Gcreate(file_id, GROUPNAME, 0, H5P_DEFAULT, H5P_DEFAULT);
  H5CHECK_ERROR(group_id, "H5Gopen");

  /* Create Dataset, Write Data, Close Dataset */
  dataset_id = H5Dcreate(group_id, ArrayNames, H5T_NATIVE_DOUBLE, file_space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5CHECK_ERROR(dataset_id, "H5Dcreate");

  /* create a data space for particles local to this process */
  mem_space_id = H5Screate_simple(2, localdim, NULL);
  H5CHECK_ERROR(mem_space_id, "H5Screate_simple");

  /* select a hyperslab into the filespace for our local particles */
  status = H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, offset, NULL, localdim, NULL );
  H5CHECK_ERROR(status, "H5Sselect_hyperslab");

  /* Create Dataset, Write Data, Close Dataset */
  if (dsmBuffer) {
    status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, mem_space_id, file_space_id, H5P_DEFAULT, (*buf).Ddata);
    H5CHECK_ERROR(status, "H5Dwrite");
  } else {
    xfer_plist_id = H5Pcreate(H5P_DATASET_XFER);
    H5CHECK_ERROR(xfer_plist_id, "H5Pcreate(H5P_DATASET_XFER)");
    H5Pset_dxpl_mpio(xfer_plist_id, H5FD_MPIO_COLLECTIVE);
    status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, mem_space_id, file_space_id, xfer_plist_id, (*buf).Ddata);
    H5CHECK_ERROR(status, "H5Dwrite");
    status = H5Pclose(xfer_plist_id);
    H5CHECK_ERROR(status, "H5Pclose(xfer_plist_id)");
  }
  H5CHECK_ERROR(status, "H5Dwrite");

  /* Release resources */
  status = H5Sclose(mem_space_id);
  H5CHECK_ERROR(status, "H5Sclose");
  status = H5Dclose(dataset_id);
  H5CHECK_ERROR(status, "H5Dclose");
  status = H5Sclose(file_space_id);
  H5CHECK_ERROR(status, "H5Sclose");
  status = H5Gclose(group_id);
  H5CHECK_ERROR(status, "H5Gclose");
  status = H5Fclose(file_id);
  H5CHECK_ERROR(status, "H5Fclose");
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
  doublearray = (H5FDdsmFloat64*)malloc(sizeof(H5FDdsmFloat64)*3*N);
  for (i=0; i<N; i++) {
    doublearray[3*i]   = 0.1*i;
    doublearray[3*i+1] = 0.1*i;
    doublearray[3*i+2] = 0.1*i;
  }
  WriteBuffer.Ddata = doublearray;

  // call the write routine with our dummy buffer
  MPI_Barrier(dcomm);
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
      H5FDdsmUInt64 numParticles;
      numParticles = (H5FDdsmUInt64) ((1024 * 1024 * remoteMB - H5FD_DSM_ALIGNMENT) /
          (sizeof(H5FDdsmFloat64) * 3.0 * dsmManager->GetUpdateNumPieces()));
      if (dsmManager->GetDSMHandle()->GetDsmType() == H5FD_DSM_TYPE_DYNAMIC_MASK) {
        dsmManager->GetDSMHandle()->SetMaskLength(numParticles * sizeof(H5FDdsmFloat64) * 3.0 * dsmManager->GetUpdateNumPieces());
        dsmManager->GetDSMHandle()->SendMaskLength();
      }
      Bytes       = numParticles * sizeof(H5FDdsmFloat64) * 3.0; // 3 = {x,y,z}
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
                    << "NumArrays, "        << 3            << ", "
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
