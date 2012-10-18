/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmTestDriver.h

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


#ifndef __H5FDdsmTestDriver_h
#define __H5FDdsmTestDriver_h

#include <string>
#include <vector>

#include <H5VLdso_sys/Process.h>

class H5FDdsmTestDriver
{
public:
  int Main(int argc, char* argv[]);
  H5FDdsmTestDriver();
  ~H5FDdsmTestDriver();

protected:
  void SeparateArguments(const char* str, 
                         std::vector<std::string>& flags);
  
  void ReportCommand(const char* const* command, const char* name);
  int ReportStatus(H5VLdso_sysProcess* process, const char* name);
  int ProcessCommandLine(int argc, char* argv[]);
  void CollectConfiguredOptions();
  void CreateCommandLine(std::vector<const char*>& commandLine,
                         const char* paraView,
                         const char* numProc,
                         int argStart=0,
                         int argCount=0,
                         char* argv[]=0);
  
  int StartServer(H5VLdso_sysProcess* server, const char* name,
                  std::vector<char>& out, std::vector<char>& err);
  int StartClient(H5VLdso_sysProcess* client, const char* name);
  void Stop(H5VLdso_sysProcess* p, const char* name);
  int OutputStringHasError(const char* pname, std::string& output);

  int WaitForLine(H5VLdso_sysProcess* process, std::string& line, double timeout,
                  std::vector<char>& out, std::vector<char>& err);
  void PrintLine(const char* pname, const char* line);
  int WaitForAndPrintLine(const char* pname, H5VLdso_sysProcess* process,
                          std::string& line, double timeout,
                          std::vector<char>& out, std::vector<char>& err,
                          int* foundWaiting);

  std::string GetDirectory(std::string location);

private:
  std::string ClientExecutable;  // fullpath to paraview executable
  std::string ServerExecutable;  // fullpath to paraview server executable
  std::string MPIRun;  // fullpath to mpirun executable

  // This specify the preflags and post flags that can be set using:
  // VTK_MPI_PRENUMPROC_FLAGS VTK_MPI_PREFLAGS / VTK_MPI_POSTFLAGS at config time
  std::vector<std::string> MPIPreNumProcFlags;
  std::vector<std::string> MPIPreFlags;
  std::vector<std::string> MPIPostFlags;
  
  // Specify the number of process flag, this can be set using: VTK_MPI_NUMPROC_FLAG. 
  // This is then split into : 
  // MPIServerNumProcessFlag & MPIRenderServerNumProcessFlag
  std::string MPINumProcessFlag;
  std::string MPIServerNumProcessFlag;

  std::string CurrentPrintLineName;

  double TimeOut;
  double ServerExitTimeOut; // time to wait for servers to finish.
  int TestServer;

  int ArgStart;
  int AllowErrorInOutput;
};

#endif // __H5FDdsmTestDriver_h

