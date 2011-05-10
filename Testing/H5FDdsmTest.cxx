/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmTest.cxx

  Authors:
     John Biddiscombe     Jerome Soumagne
     biddisco@cscs.ch     soumagne@cscs.ch

  Copyright (C) CSCS - Swiss National Supercomputing Centre.
  You may use modify and and distribute this code freely providing
  1) This copyright notice appears on all copies of source code
  2) An acknowledgment appears with any substantial usage of the code
  3) If this code is contributed to any other open source project, it
  must not be reformatted such that the indentation, bracketing or
  overall style is modified significantly.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  This work has received funding from the European Community's Seventh
  Framework Programme (FP7/2007-2013) under grant agreement 225967 “NextMuSE”

=========================================================================*/

#include "H5FDdsmTest.h"
#include <hdf5.h>
#include "H5FDdsm.h"

#include <cstdlib>

//----------------------------------------------------------------------------
/* names of above arrays for convenience when using H5Part IO */
H5FDdsmConstString ArrayNames = "Position";
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
void initBuffer(ParticleBuffer_t *buffer) {
  (*buffer).Ddata = NULL;
}

//----------------------------------------------------------------------------
void freeBuffer(ParticleBuffer_t *buffer) {
  if ((*buffer).Ddata) free((*buffer).Ddata);
}

//----------------------------------------------------------------------------
/*******************************************************************/
/* N   = number this process will write                            */
/* C   = number of components, {vector C=3, x,y,z}                 */
/* start = start index for this write                              */
/* total = total to be written by all all processes                */
/* collective : 1=collective IO, 0=independent (default 0)         */
/*******************************************************************/
void WriteParticlesHDF5(
    ParticleBuffer *buf, H5FDdsmConstString filename,
    H5FDdsmUInt64 N, H5FDdsmUInt64 C, H5FDdsmUInt64 start, H5FDdsmUInt64 total, H5FDdsmBuffer *dsmBuffer)
{
  hid_t      file_id, group_id, dataset_id, xfer_plist_id;
  hid_t      file_space_id, mem_space_id;
  hid_t      acc_plist_id;
  hsize_t    count[2]    = {total, C};
  hsize_t    offset[2]   = {start, 0};
  hsize_t    localdim[2] = {N,     C};
  herr_t     status = 0;

  /* Set up file access property list with parallel I/O */
  acc_plist_id = H5Pcreate(H5P_FILE_ACCESS);
  H5CHECK_ERROR(acc_plist_id, "H5Pcreate(H5P_FILE_ACCESS)");
  if (dsmBuffer) {
    H5FD_dsm_init();
    H5Pset_fapl_dsm(acc_plist_id, MPI_COMM_WORLD, dsmBuffer);
    H5CHECK_ERROR(status, "H5Pset_fapl_dsm");
    status = H5Pset_alignment(acc_plist_id, dsmBuffer->GetBlockLength()/2, dsmBuffer->GetBlockLength());
    if (dsmBuffer->GetBlockLength() > 0) {
      std::cout << "H5P Alignment : " << std::hex << dsmBuffer->GetBlockLength() << std::endl;
      H5CHECK_ERROR(acc_plist_id, "h5pset_alignment");
    }
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

/*******************************************************************/
/* N   = number this process will write                            */
/* C   = number of components, {vector C=3, x,y,z}                 */
/* start = start index for this write                              */
/* total = total to be written by all all processes                */
/*******************************************************************/
void WriteParticlesDSM(
    ParticleBuffer *buf, H5FDdsmConstString filename,
    H5FDdsmUInt64 N, H5FDdsmUInt64 C, H5FDdsmUInt64 start, H5FDdsmUInt64 total, H5FDdsmBuffer *dsmBuffer)
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
  if (dsmBuffer->Put(start*sizeof(H5FDdsmFloat64), N*C*sizeof(H5FDdsmFloat64), (void *) (*buf).Ddata) != H5FD_DSM_SUCCESS) {
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
// Test Particle write 
// N = numtuples
// C = numComponents
//----------------------------------------------------------------------------
H5FDdsmFloat64 TestParticleWrite(
    H5FDdsmConstString filename, H5FDdsmUInt64 N, H5FDdsmUInt64 C, 
    H5FDdsmInt32 mpiId, H5FDdsmInt32 mpiNum,
    MPI_Comm dcomm, H5FDdsmBuffer *dsmBuffer, FuncPointer pointer)
{
  ParticleBuffer WriteBuffer;
  H5FDdsmUInt64 i, start, total;
  H5FDdsmFloat64 *doublearray;
  static H5FDdsmInt32 step_increment = 0;

  start = N*mpiId;
  total = N*mpiNum;
  // set all array pointers to zero
  initBuffer(&WriteBuffer);

  // create arrays for the test vars we selected above
  doublearray = (H5FDdsmFloat64*)malloc(sizeof(H5FDdsmFloat64)*C*N);
  for (i=0; i<N; i++) {
    for (int c=0; c<C; c++) {
      doublearray[C*i+c] = static_cast<H5FDdsmFloat64>(i + start + step_increment);
    }
  }
  WriteBuffer.Ddata = doublearray;

  // call the write routine with our dummy buffer
  MPI_Barrier(dcomm);
  H5FDdsmFloat64 t1 = MPI_Wtime();
  pointer(&WriteBuffer, filename, N, C, start, total, dsmBuffer);
  MPI_Barrier(dcomm);
  H5FDdsmFloat64 t2 = MPI_Wtime();

  // free all array pointers
  freeBuffer(&WriteBuffer);
  step_increment++;
  return t2-t1;
};

//----------------------------------------------------------------------------
void receiverInit(int argc, char* argv[], H5FDdsmManager *dsmManager, MPI_Comm *comm)
{
  H5FDdsmInt32 provided, rank, size;
  H5FDdsmUInt32 dsmSize = 16; // default MB
  H5FDdsmInt32 commType = H5FD_DSM_COMM_SOCKET;
  H5FDdsmInt32 dsmType = H5FD_DSM_TYPE_UNIFORM;
  H5FDdsmUInt64 dsmBlockSize = 1024;
  H5FDdsmBoolean staticInterComm = false;
  //
  // Receiver will spawn a thread to handle incoming data Put/Get requests
  // we must therefore have MPI_THREAD_MULTIPLE
  //
  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
  MPI_Comm_rank(*comm, &rank);
  MPI_Comm_size(*comm, &size);
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

  //
  // Pause for debugging
  //
#ifdef H5FD_TEST_WAIT
  if (rank == 0) {
    std::cout << "Attach debugger if necessary, then press <enter>" << std::endl;
    char c;
    std::cin >> c;
  }
#endif

  if (argc > 1) {
    dsmSize = atol(argv[1]);
  }

  if (argc > 2) {
    if (!strcmp(argv[2], "Socket")) {
      commType = H5FD_DSM_COMM_SOCKET;
      if (rank == 0) std::cout << "# SOCKET Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "MPI")) {
      commType = H5FD_DSM_COMM_MPI;
      if (rank == 0) std::cout << "# MPI Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "MPI_RMA")) {
      commType = H5FD_DSM_COMM_MPI_RMA;
      if (rank == 0) std::cout << "# MPI_RMA Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "DMAPP")) {
      commType = H5FD_DSM_COMM_DMAPP;
      staticInterComm = true;
      if (rank == 0) std::cout << "# DMAPP Inter-Communicator selected" << std::endl;
    }
  }

  if (argc > 3) {
    if (!strcmp(argv[3], "Static") && (commType != H5FD_DSM_COMM_SOCKET)) {
      staticInterComm = true;
    }
  }

  if (staticInterComm) {
    H5FDdsmInt32 color = 1; // 1 for server, 2 for client
    MPI_Comm_split(MPI_COMM_WORLD, color, rank, comm);
    MPI_Comm_rank(*comm, &rank);
    MPI_Comm_size(*comm, &size);
  }

  if (argc > 4) {
    if (!strcmp(argv[4], "Block")) {
      dsmType = H5FD_DSM_TYPE_BLOCK_CYCLIC;
      if (rank == 0) std::cout << "# Block Cyclic redistribution selected" << std::endl;
    }
    else if (!strcmp(argv[4], "Mask")) {
      dsmType = H5FD_DSM_TYPE_DYNAMIC_MASK;
    }
  }

  if (argc > 5) {
    dsmBlockSize = atol(argv[5]);
  }

  //std::cout << "Process number " << rank << " of " << size - 1 << std::endl;

  //
  // Create a DSM manager
  //
  dsmManager->SetCommunicator(*comm);
  dsmManager->SetLocalBufferSizeMBytes(dsmSize/size);
  dsmManager->SetDsmType(dsmType);
  dsmManager->SetDsmBlockLength(dsmBlockSize);
  dsmManager->SetDsmCommType(commType);
  dsmManager->SetDsmIsServer(1);
  if (staticInterComm) dsmManager->SetDsmUseStaticInterComm(1);
  dsmManager->SetServerHostName("default");
  dsmManager->SetServerPort(22000);
  dsmManager->CreateDSM();

  if (staticInterComm) {
    dsmManager->ConnectInterCommDSM();
  } else {
    // Publish writes .dsm_config file with server name/port/mode in
    // then spawns thread which waits for incoming connections
    dsmManager->PublishDSM();
  }
  //
  H5FDdsmFloat64 totalMB = (H5FDdsmFloat64) (dsmManager->GetDSMHandle()->GetTotalLength()/(1024*1024));
  H5FDdsmUInt32 serversize = (dsmManager->GetDSMHandle()->GetEndServerId() -
      dsmManager->GetDSMHandle()->GetStartServerId() + 1);
//  if (rank == 0) {
//    std::cout << "# DSM server memory size is: " << totalMB << " MBytes"
//        << " (" << dsmManager->GetDSMHandle()->GetTotalLength() << " Bytes)" << std::endl;
//    std::cout << "# DSM server process count: " <<  serversize << std::endl;
//    if (dsmType == H5FD_DSM_TYPE_BLOCK_CYCLIC) std::cout << "Block size: "
//        <<  dsmManager->GetDSMHandle()->GetBlockLength() << " Bytes" << std::endl;
//  }

  // The output comment below must not be deleted, it allows ctest to detect
  // when the server is initialized
  MPI_Barrier(*comm);
  if (rank == 0) std::cout << "# Waiting for client..." << std::endl;
  dsmManager->WaitForConnected();
}

//----------------------------------------------------------------------------
void receiverFinalize(H5FDdsmManager *dsmManager, MPI_Comm *comm)
{
  H5FDdsmBoolean staticInterComm = dsmManager->GetDsmUseStaticInterComm();
  // std::cout << "Process number " << rank << " Closing down DSM server" << std::endl;
  //
  // Sync here
  //
  MPI_Barrier(dsmManager->GetCommunicator());
  //
  // Closes ports or MPI communicators
  //
  if (!staticInterComm) dsmManager->UnpublishDSM();
  //
  // Clean up everything
  //
  dsmManager->DestroyDSM();
  if (staticInterComm) MPI_Comm_free(comm);
  MPI_Finalize();
}

//----------------------------------------------------------------------------
void senderInit(int argc, char* argv[], H5FDdsmManager *dsmManager, MPI_Comm *comm, H5FDdsmInt32 *dataSizeMB)
{
  H5FDdsmInt32   nlocalprocs, rank;
  H5FDdsmBoolean staticInterComm = false;
  H5FDdsmFloat64 remoteMB;
  H5FDdsmUInt32  numServers;
  //
  // Sender does not use any special threads so MPI_Init is ok
  //
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(*comm,&rank);
  MPI_Comm_size(*comm,&nlocalprocs);

  //
  // Pause for debugging
  //
#ifdef H5FD_TEST_WAIT
  if (rank == 0) {
    std::cout << "Attach debugger if necessary, then press <enter>" << std::endl;
    char c;
    std::cin >> c;
  }
  MPI_Barrier(dcomm);
#endif

  if (argc > 1) {
    if (dataSizeMB != NULL) *dataSizeMB = atoi(argv[1]);
  }

  if (argc > 2) {
    // If an argument to give the comm is passed, assume we use static mode for now
    H5FDdsmInt32 commType = H5FD_DSM_COMM_MPI;
    if (!strcmp(argv[2], "MPI")) {
      commType = H5FD_DSM_COMM_MPI;
    }
    else if (!strcmp(argv[2], "MPI_RMA")) {
      commType = H5FD_DSM_COMM_MPI_RMA;
    }
    else if (!strcmp(argv[2], "DMAPP")) {
      commType = H5FD_DSM_COMM_DMAPP;
      staticInterComm = true;
    }
    dsmManager->SetDsmCommType(commType);
    dsmManager->SetDsmUseStaticInterComm(1);
  } else {
    dsmManager->ReadDSMConfigFile();
  }
  if (dsmManager->GetDsmUseStaticInterComm()) {
    H5FDdsmInt32 color = 2; // 1 for server, 2 for client
    MPI_Comm_split(MPI_COMM_WORLD, color, rank, comm);
    MPI_Comm_rank(*comm, &rank);
    MPI_Comm_size(*comm, &nlocalprocs);
    staticInterComm = true;
  }
  //
  // Create a DSM manager
  //
  dsmManager->SetCommunicator(*comm);
  dsmManager->SetDsmIsServer(0);
  dsmManager->CreateDSM();

  if (staticInterComm) {
    dsmManager->ConnectInterCommDSM();
  } else {
    // Connect to receiver
    dsmManager->ConnectDSM(true);
  }
  //
  // Get info from remote server
  //
  remoteMB = dsmManager->GetDSMHandle()->GetTotalLength() / (1024.0 * 1024.0);
  numServers = dsmManager->GetDSMHandle()->GetEndServerId() - dsmManager->GetDSMHandle()->GetStartServerId() + 1;
  if (rank == 0) {
    std::cout << "# DSM server memory size is: " << remoteMB << " MBytes"
        << " (" << dsmManager->GetDSMHandle()->GetTotalLength() << " Bytes)" << std::endl;
    std::cout << "# DSM server process count: " <<  numServers << std::endl;
    if (dsmManager->GetDSMHandle()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_CYCLIC) {
      std::cout << "# Block size: " <<  dsmManager->GetDSMHandle()->GetBlockLength() << " Bytes" << std::endl;
    }
  }
}

//----------------------------------------------------------------------------
void senderFinalize(H5FDdsmManager *dsmManager, MPI_Comm *comm)
{
  H5FDdsmBoolean staticInterComm = dsmManager->GetDsmUseStaticInterComm();
  dsmManager->DisconnectDSM();
  //
  dsmManager->DestroyDSM();
  if (staticInterComm) MPI_Comm_free(comm);
  MPI_Finalize();
}
