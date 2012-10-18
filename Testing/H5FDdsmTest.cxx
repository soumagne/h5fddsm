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
#include "H5FDdsm.h"
#if H5_VERSION_GE(1,9,0)
 #include "H5VLdso.h"
#endif

#include "H5Ppublic.h"
#include "H5Spublic.h"
#include "H5Spublic.h"
#include "H5FDmpi.h"
#include "H5FDmpio.h"

#include <cstdlib>
#include <string>

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
void particleWriteHdf(ParticleBuffer *buf, H5FDdsmConstString filename,
    H5FDdsmUInt64 ntuples, H5FDdsmUInt64 ncomponents, H5FDdsmUInt32 ndatasets,
    H5FDdsmUInt64 start, H5FDdsmUInt64 total, MPI_Comm comm, H5FDdsmManager *dsmManager)
{
  hid_t      file_id, group_id, dataset_id, xfer_plist_id;
  hid_t      file_space_id, mem_space_id;
  hid_t      acc_plist_id;
  hsize_t    count[2]    = {total, ncomponents};
  hsize_t    offset[2]   = {start, 0};
  hsize_t    localdim[2] = {ntuples, ncomponents};
  int        rank = (ncomponents == 1) ? 1 : 2;
  herr_t     status = 0;

  /* Set up file access property list with parallel I/O */
  acc_plist_id = H5Pcreate(H5P_FILE_ACCESS);
  H5CHECK_ERROR(acc_plist_id, "H5Pcreate(H5P_FILE_ACCESS)");
  if (dsmManager) {
#if H5_VERSION_GE(1,9,0)
    H5Pset_fapl_dso(acc_plist_id);
#endif
    H5Pset_fapl_dsm(acc_plist_id, comm, NULL, 0);
    H5CHECK_ERROR(status, "H5Pset_fapl_dsm");
  } else {
    H5Pset_fapl_mpio(acc_plist_id, comm, MPI_INFO_NULL);
    H5CHECK_ERROR(status, "H5Pset_fapl_mpio");
  }

  /* Create a new file collectively (overwrite existing) */
  file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_plist_id);
  H5CHECK_ERROR(file_id, "H5Fcreate");

  /* Close property list */
  status = H5Pclose(acc_plist_id);
  H5CHECK_ERROR(status, "H5Pclose(acc_plist_id)");

  /* Create the file_space_id */
  file_space_id = H5Screate_simple(rank, count, NULL);
  H5CHECK_ERROR(file_space_id, "H5Screate_simple");

  // Create a group to hold this time step (for H5Part compatibility)
  group_id = H5Gcreate(file_id, GROUPNAME, 0, H5P_DEFAULT, H5P_DEFAULT);
  H5CHECK_ERROR(group_id, "H5Gopen");

  for (H5FDdsmUInt32 dataSetIndex = 0; dataSetIndex < ndatasets; dataSetIndex++) {
    std::string dataSetName;

    if (ndatasets > 1) {
      char dataSetIndexString[16];
      sprintf(dataSetIndexString, "%d", dataSetIndex);
      dataSetName = std::string(DATASETNAME) + std::string(dataSetIndexString);
    } else {
      dataSetName = DATASETNAME;
    }

    /* Create Dataset, Write Data, Close Dataset */
    dataset_id = H5Dcreate(group_id, dataSetName.c_str(), H5T_NATIVE_DOUBLE,
        file_space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5CHECK_ERROR(dataset_id, "H5Dcreate");

    /* create a data space for particles local to this process */
    mem_space_id = H5Screate_simple(rank, localdim, NULL);
    H5CHECK_ERROR(mem_space_id, "H5Screate_simple");

    /* select a hyperslab into the filespace for our local particles */
    status = H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, offset,
        NULL, localdim, NULL );
    H5CHECK_ERROR(status, "H5Sselect_hyperslab");

    /* Create Dataset, Write Data, Close Dataset */
    if (dsmManager) {
      status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, mem_space_id,
          file_space_id, H5P_DEFAULT, (*buf).Ddata);
      H5CHECK_ERROR(status, "H5Dwrite");
    } else {
      xfer_plist_id = H5Pcreate(H5P_DATASET_XFER);
      H5CHECK_ERROR(xfer_plist_id, "H5Pcreate(H5P_DATASET_XFER)");
      H5Pset_dxpl_mpio(xfer_plist_id, H5FD_MPIO_COLLECTIVE);
      status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, mem_space_id,
          file_space_id, xfer_plist_id, (*buf).Ddata);
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
  }
  status = H5Sclose(file_space_id);
  H5CHECK_ERROR(status, "H5Sclose");
  status = H5Gclose(group_id);
  H5CHECK_ERROR(status, "H5Gclose");
  status = H5Fclose(file_id);
  H5CHECK_ERROR(status, "H5Fclose");
}

//----------------------------------------------------------------------------
void particleReadHdf(ParticleBuffer *buf, H5FDdsmConstString filename,
    H5FDdsmUInt64 ntuples, H5FDdsmUInt64 ncomponents, H5FDdsmUInt32 ndatasets,
    H5FDdsmUInt64 start, H5FDdsmUInt64 total, MPI_Comm comm, H5FDdsmManager *dsmManager)
{
  hid_t      file_id, group_id, dataset_id;
  hid_t      file_space_id, mem_space_id;
  hid_t      acc_plist_id;
  hsize_t    count[2]    = {total, ncomponents};
  hsize_t    offset[2]   = {start, 0};
  hsize_t    localdim[2] = {ntuples, ncomponents};
  int        rank = (ncomponents == 1) ? 1 : 2;
  herr_t     status = 0;

  // Set up file access property list with parallel I/O
  acc_plist_id = H5Pcreate(H5P_FILE_ACCESS);
  H5CHECK_ERROR(acc_plist_id, "H5Pcreate(H5P_FILE_ACCESS)");
  if (dsmManager) {
    H5Pset_fapl_dsm(acc_plist_id, dsmManager->GetMpiComm(), NULL, 0);
    H5CHECK_ERROR(status, "H5Pset_fapl_dsm");
  }

  // Create a new file collectively (overwrite existing)
  file_id = H5Fopen(filename, H5F_ACC_RDONLY, acc_plist_id);
  H5CHECK_ERROR(file_id, "H5Fopen");

  // Close property list
  status = H5Pclose(acc_plist_id);
  H5CHECK_ERROR(status, "H5Pclose(acc_plist_id)");

  // Create the file_space_id
  file_space_id = H5Screate_simple(rank, count, NULL);
  H5CHECK_ERROR(file_space_id, "H5Screate_simple");

  // Open the group
  group_id = H5Gopen(file_id, GROUPNAME, H5P_DEFAULT);
  H5CHECK_ERROR(group_id, "H5Gopen");

  for (H5FDdsmUInt32 dataSetIndex = 0; dataSetIndex < ndatasets; dataSetIndex++) {
    std::string dataSetName;

    if (ndatasets > 1) {
      char dataSetIndexString[16];
      sprintf(dataSetIndexString, "%d", dataSetIndex);
      dataSetName = std::string(DATASETNAME) + std::string(dataSetIndexString);
    } else {
      dataSetName = DATASETNAME;
    }

    // Open the dataset
    dataset_id = H5Dopen(group_id, dataSetName.c_str(), H5P_DEFAULT);
    H5CHECK_ERROR(dataset_id, "H5Dopen");

    // create a data space for particles local to this process */
    mem_space_id = H5Screate_simple(rank, localdim, NULL);
    H5CHECK_ERROR(mem_space_id, "H5Screate_simple");

    // select a hyperslab into the filespace for our local particles */
    status = H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, offset,
        NULL, localdim, NULL );
    H5CHECK_ERROR(status, "H5Sselect_hyperslab");

    // Read the dataset
    status = H5Dread(dataset_id, H5T_NATIVE_DOUBLE, mem_space_id, file_space_id,
        H5P_DEFAULT, (*buf).Ddata);
    H5CHECK_ERROR(dataset_id, "H5Dread");

    // Release resources
    status = H5Sclose(mem_space_id);
    H5CHECK_ERROR(status, "H5Sclose");
    status = H5Dclose(dataset_id);
    H5CHECK_ERROR(status, "H5Dclose");
  }
  status = H5Sclose(file_space_id);
  H5CHECK_ERROR(status, "H5Sclose");
  status = H5Gclose(group_id);
  H5CHECK_ERROR(status, "H5Gclose");
  status = H5Fclose(file_id);
  H5CHECK_ERROR(status, "H5Fclose");
}

//----------------------------------------------------------------------------
H5FDdsmFloat64 TestParticleWrite(H5FDdsmConstString filename, H5FDdsmUInt64 ntuples,
    H5FDdsmUInt64 ncomponents, H5FDdsmUInt32 ndatasets, H5FDdsmInt32 rank, H5FDdsmInt32 size,
    MPI_Comm comm, H5FDdsmManager *dsmManager, FuncPointer pointer)
{
  ParticleBuffer WriteBuffer;
  H5FDdsmUInt64 start, total;
  H5FDdsmFloat64 *doublearray;
  static H5FDdsmInt32 step_increment = 0;

  start = ntuples * rank;
  total = ntuples * size;
  // set all array pointers to zero
  initBuffer(&WriteBuffer);

  // create arrays for the test vars we selected above
  doublearray = (H5FDdsmFloat64*) malloc(sizeof(H5FDdsmFloat64) * ncomponents * ntuples);
  for (H5FDdsmUInt64 i = 0; i < ntuples; i++) {
    for (H5FDdsmUInt64 j = 0; j < ncomponents; j++) {
      doublearray[ncomponents * i + j] = static_cast<H5FDdsmFloat64>(i + start + step_increment);
    }
  }
  WriteBuffer.Ddata = doublearray;

  // call the write routine with our dummy buffer
  MPI_Barrier(comm);
  H5FDdsmFloat64 t1 = MPI_Wtime();
  pointer(&WriteBuffer, filename, ntuples, ncomponents, ndatasets, start, total,
      comm, dsmManager);
  MPI_Barrier(comm);
  H5FDdsmFloat64 t2 = MPI_Wtime();

  // free all array pointers
  freeBuffer(&WriteBuffer);
  step_increment++;
  return(t2 - t1);
};

//----------------------------------------------------------------------------
H5FDdsmFloat64 TestParticleRead(H5FDdsmConstString filename, H5FDdsmUInt64 ntuples,
    H5FDdsmUInt64 ncomponents, H5FDdsmUInt32 ndatasets, H5FDdsmInt32 rank, H5FDdsmInt32 size,
    MPI_Comm comm, H5FDdsmManager *dsmManager, bool checkresults)
{
  ParticleBuffer_t ReadBuffer;
  H5FDdsmUInt64 start, total;
  H5FDdsmFloat64 *doublearray;
  static H5FDdsmInt32 step_increment = 0;
  H5FDdsmInt32 fail_count = 0;

  start = ntuples * rank;
  total = ntuples * size;
  // set all array pointers to zero
  initBuffer(&ReadBuffer);

  // create arrays for the test vars we selected above
  doublearray = (H5FDdsmFloat64*) malloc(sizeof(H5FDdsmFloat64) * ncomponents * ntuples);
  for (H5FDdsmUInt64 i = 0; i < ntuples; i++) {
    for (H5FDdsmUInt64 j = 0; j < ncomponents; j++) {
      doublearray[ncomponents * i + j] = 0;
    }
  }
  ReadBuffer.Ddata = doublearray;

  // call the write routine with our dummy buffer
  MPI_Barrier(comm);
  H5FDdsmFloat64 t1 = MPI_Wtime();
  particleReadHdf(&ReadBuffer, filename, ntuples, ncomponents, ndatasets, start,
      total, comm, dsmManager);
  MPI_Barrier(comm);
  H5FDdsmFloat64 t2 = MPI_Wtime();

  if (checkresults && step_increment < 5) {
    /* Check the results the first times. */
    for (H5FDdsmUInt64 i = 0; i < ntuples; i++) {
      for (H5FDdsmUInt64 j = 0; j < ncomponents; j++) {
        if ((doublearray[ncomponents * i + j] != static_cast<H5FDdsmFloat64>
        (i + start + step_increment)) && (fail_count < 10)) {
          fprintf(stderr," doublearray[%llu] is %lf, should be %lf\n", ncomponents * i + j,
              doublearray[ncomponents * i + j], static_cast<H5FDdsmFloat64>(i + start + step_increment));
          fail_count++;
        }
      }
    }
    if (fail_count == 0) {
      if ((rank == 0) && (step_increment == 0)) printf("# DSM read test PASSED for step 0\n");
    } else {
      fprintf(stderr,"DSM read test FAILED for PE %d, %d or more wrong values at step %d\n",
          rank, fail_count, step_increment);
    }
  }
  // free all array pointers
  freeBuffer(&ReadBuffer);
  step_increment++;

  MPI_Allreduce(MPI_IN_PLACE, &fail_count, 1, MPI_INT, MPI_SUM, comm);
  if (fail_count > 0)
    return(H5FD_DSM_FAIL);
  else
    return(t2 - t1);
};

//----------------------------------------------------------------------------
void receiverInit(int argc, char* argv[], H5FDdsmManager *dsmManager, MPI_Comm *comm)
{
  H5FDdsmInt32 provided, rank, size;
  H5FDdsmUInt32 dsmSize = 16; // default MB
  H5FDdsmInt32 commType = H5FD_DSM_COMM_SOCKET;
  H5FDdsmInt32 dsmType = H5FD_DSM_TYPE_UNIFORM;
  H5FDdsmUInt64 dsmBlockSize = 1024;
  H5FDdsmBoolean staticInterComm = H5FD_DSM_FALSE;
  //
  // Receiver will spawn a thread to handle incoming data Put/Get requests
  // we must therefore have MPI_THREAD_MULTIPLE
  //
  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
  // MPI_Init_thread(&argc, &argv, MPI_THREAD_SERIALIZED, &provided);
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
      dsmBlockSize = 32768;
      if (rank == 0) std::cout << "# MPI Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "MPI_RMA")) {
      commType = H5FD_DSM_COMM_MPI_RMA;
      dsmBlockSize = 32768;
      if (rank == 0) std::cout << "# MPI_RMA Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "DMAPP")) {
      commType = H5FD_DSM_COMM_DMAPP;
      staticInterComm = H5FD_DSM_TRUE;
      dsmBlockSize = 2048;
      if (rank == 0) std::cout << "# DMAPP Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "UGNI")) {
      commType = H5FD_DSM_COMM_UGNI;
      staticInterComm = H5FD_DSM_TRUE;
      if (rank == 0) std::cout << "# UGNI Inter-Communicator selected" << std::endl;
    }
  }

  if (argc > 3) {
    if (!strcmp(argv[3], "Static") && (commType != H5FD_DSM_COMM_SOCKET)) {
      staticInterComm = H5FD_DSM_TRUE;
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
    else if (!strcmp(argv[4], "RBlock")) {
      dsmType = H5FD_DSM_TYPE_BLOCK_RANDOM;
      if (rank == 0) std::cout << "# Random Block redistribution selected" << std::endl;
    }
  }

  if (argc > 5) {
    dsmBlockSize = atol(argv[5]);
  }

  //std::cout << "Process number " << rank << " of " << size - 1 << std::endl;

  //
  // Create a DSM manager
  //
  dsmManager->SetMpiComm(*comm);
  dsmManager->SetLocalBufferSizeMBytes(dsmSize/size);
  dsmManager->SetDsmType(dsmType);
  dsmManager->SetBlockLength(dsmBlockSize);
  dsmManager->SetInterCommType(commType);
  dsmManager->SetIsServer(H5FD_DSM_TRUE);
  if (staticInterComm) dsmManager->SetUseStaticInterComm(H5FD_DSM_TRUE);
  dsmManager->SetServerHostName("default");
  dsmManager->SetServerPort(22000);
  dsmManager->Create();
  H5FD_dsm_set_manager(dsmManager);

  // Publish writes .dsm_config file with server name/port/mode in
  // then spawns thread which waits for incoming connections
  dsmManager->Publish();

  //
//  H5FDdsmFloat64 totalMB = (H5FDdsmFloat64) (dsmManager->GetDSMHandle()->GetTotalLength()/(1024*1024));
//  H5FDdsmUInt32 serversize = (dsmManager->GetDSMHandle()->GetEndServerId() -
//      dsmManager->GetDSMHandle()->GetStartServerId() + 1);
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
  dsmManager->WaitForConnection();
}

//----------------------------------------------------------------------------
void receiverFinalize(H5FDdsmManager *dsmManager, MPI_Comm *comm)
{
  H5FDdsmBoolean staticInterComm = dsmManager->GetUseStaticInterComm();
  // std::cout << "Process number " << rank << " Closing down DSM server" << std::endl;
  //
  // Sync here
  //
  MPI_Barrier(dsmManager->GetMpiComm());
  //
  // Closes ports or MPI communicators
  //
  if (!staticInterComm) dsmManager->Unpublish();
  //
  // Clean up everything
  //
  dsmManager->Destroy();
  if (staticInterComm) {
    MPI_Barrier(*comm);
    MPI_Comm_free(comm);
  }
  MPI_Finalize();
}

//----------------------------------------------------------------------------
void senderInit(int argc, char* argv[], H5FDdsmManager *dsmManager, MPI_Comm *comm,
    H5FDdsmInt32 *dataSizeMB)
{
  H5FDdsmInt32   nlocalprocs, rank;
  H5FDdsmBoolean staticInterComm = H5FD_DSM_FALSE;
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
    }
    else if (!strcmp(argv[2], "UGNI")) {
      commType = H5FD_DSM_COMM_UGNI;
    }
    dsmManager->SetInterCommType(commType);
    dsmManager->SetUseStaticInterComm(H5FD_DSM_TRUE);
  } else {
    dsmManager->ReadConfigFile();
  }
  if (dsmManager->GetUseStaticInterComm()) {
    H5FDdsmInt32 color = 2; // 1 for server, 2 for client
    MPI_Comm_split(MPI_COMM_WORLD, color, rank, comm);
    MPI_Comm_rank(*comm, &rank);
    MPI_Comm_size(*comm, &nlocalprocs);
    staticInterComm = H5FD_DSM_TRUE;
  }
  //
  // Create a DSM manager
  //
  dsmManager->SetMpiComm(*comm);
  dsmManager->SetIsServer(H5FD_DSM_FALSE);
  dsmManager->Create();
  H5FD_dsm_set_manager(dsmManager);

  if (staticInterComm) {
    dsmManager->Connect();
  } else {
    // Connect to receiver
    dsmManager->Connect(H5FD_DSM_TRUE);
  }
  //
  // Get info from remote server
  //
  remoteMB = dsmManager->GetDsmBuffer()->GetTotalLength() / (1024.0 * 1024.0);
  numServers = dsmManager->GetDsmBuffer()->GetEndServerId() - dsmManager->GetDsmBuffer()->GetStartServerId() + 1;
  if (rank == 0) {
    printf("# DSM server memory size is: %lf MBytes (%llu Bytes)\n", remoteMB,
        dsmManager->GetDsmBuffer()->GetTotalLength());
    printf("# DSM server process count: %u\n", numServers);
    if (dsmManager->GetDsmBuffer()->GetDsmType() == H5FD_DSM_TYPE_BLOCK_CYCLIC) {
      printf("# Block size: %llu Bytes\n", dsmManager->GetDsmBuffer()->GetBlockLength());
    }
  }
}

//----------------------------------------------------------------------------
void senderFinalize(H5FDdsmManager *dsmManager, MPI_Comm *comm)
{
  H5FDdsmBoolean staticInterComm = dsmManager->GetUseStaticInterComm();
  dsmManager->Disconnect();
  //
  dsmManager->Destroy();
  if (staticInterComm) {
    MPI_Barrier(*comm);
    MPI_Comm_free(comm);
  }
  MPI_Finalize();
}
