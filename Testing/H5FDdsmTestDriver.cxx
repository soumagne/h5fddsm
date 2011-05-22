/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmTestDriver.cxx

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
#include "H5FDdsmTestDriver.h"
#include "H5FDdsmTestDriverConfig.h"

#include <cstdio>
#include <iostream>
#include <cstring>
#include <cstdlib>

#if !defined(_WIN32) || defined(__CYGWIN__)
# include <unistd.h>
# include <sys/wait.h>
#endif

#include <H5FDdsm_sys/SystemTools.hxx>

using std::vector;
using std::string;
using std::cerr;

// The main function as this class should only be used by this program
int main(int argc, char* argv[])
{
  H5FDdsmTestDriver d;
  return d.Main(argc, argv);
}
//----------------------------------------------------------------------------
H5FDdsmTestDriver::H5FDdsmTestDriver()
{
  this->AllowErrorInOutput = 0;
  this->TimeOut = 300;
  this->ServerExitTimeOut = 60;
  this->TestServer = 0;
}
//----------------------------------------------------------------------------
H5FDdsmTestDriver::~H5FDdsmTestDriver()
{
}
//----------------------------------------------------------------------------
void H5FDdsmTestDriver::SeparateArguments(const char* str,
                                     vector<string>& flags)
{
  string arg = str;
  string::size_type pos1 = 0;
  string::size_type pos2 = arg.find_first_of(" ;");
  if(pos2 == arg.npos)
    {
    flags.push_back(str);
    return;
    }
  while(pos2 != arg.npos)
    {
    flags.push_back(arg.substr(pos1, pos2-pos1));
    pos1 = pos2+1;
    pos2 = arg.find_first_of(" ;", pos1+1);
    }
  flags.push_back(arg.substr(pos1, pos2-pos1));
}
//----------------------------------------------------------------------------
void H5FDdsmTestDriver::CollectConfiguredOptions()
{
  // try to make sure that this timesout before dart so it can kill all the processes
  this->TimeOut = DART_TESTING_TIMEOUT - 10.0;
  if(this->TimeOut < 0)
    {
    this->TimeOut = 1500;
    }

  // now find all the mpi information if mpi run is set
#ifdef MPIEXEC
  this->MPIRun = MPIEXEC;
#else
  cerr << "Error: MPIEXEC must be set.\n";
  return;
#endif
  int serverNumProc = 1;

# ifdef MPIEXEC_MAX_NUMPROCS
  serverNumProc = MPIEXEC_MAX_NUMPROCS;
# endif
# ifdef MPIEXEC_NUMPROC_FLAG
  this->MPINumProcessFlag = MPIEXEC_NUMPROC_FLAG;
# else
  cerr << "Error MPIEXEC_NUMPROC_FLAG must be defined to run test.\n";
  return;
# endif
# ifdef MPIEXEC_PREFLAGS
  this->SeparateArguments(MPIEXEC_PREFLAGS, this->MPIPreFlags);
# endif
# ifdef MPIEXEC_POSTFLAGS
  this->SeparateArguments(MPIEXEC_POSTFLAGS, this->MPIPostFlags);
# endif
  char buf[1024];
  sprintf(buf, "%d", serverNumProc);
  this->MPIServerNumProcessFlag = buf;
}
//----------------------------------------------------------------------------
/// This adds the debug/build configuration crap for the executable on windows.
static string FixExecutablePath(const string& path)
{
#ifdef  CMAKE_INTDIR
  string parent_dir =
    H5FDdsm_sys::SystemTools::GetFilenamePath(path.c_str());

  string filename =
    H5FDdsm_sys::SystemTools::GetFilenameName(path);
  parent_dir += "/" CMAKE_INTDIR "/";
  return parent_dir + filename;
#endif

  return path;
}
//----------------------------------------------------------------------------
int H5FDdsmTestDriver::ProcessCommandLine(int argc, char* argv[])
{
  this->ArgStart = 1;
  int i;
  for(i =1; i < argc - 1; ++i)
    {
    if(strcmp(argv[i], "--client") == 0)
      {
      this->ArgStart = i+2;
      this->ClientExecutable = ::FixExecutablePath(argv[i+1]);
      }
    if(strcmp(argv[i], "--server") == 0)
      {
      this->ArgStart = i+2;
      this->TestServer = 1;
      this->ServerExecutable = ::FixExecutablePath(argv[i+1]);
      fprintf(stderr, "Test Server.\n");
      }
    if(strcmp(argv[i], "--timeout") == 0)
      {
      this->ArgStart = i+2;
      this->TimeOut = atoi(argv[i+1]);
      fprintf(stderr, "The timeout was set to %f.\n", this->TimeOut);
      }
    if (strncmp(argv[i], "--allow-errors", strlen("--allow-errors"))==0)
      {
      this->ArgStart =i+1;
      this->AllowErrorInOutput = 1;
      fprintf(stderr, "The allow erros in output flag was set to %d.\n", 
        this->AllowErrorInOutput);
      }
    }

  return 1;
}
//----------------------------------------------------------------------------
void
H5FDdsmTestDriver::CreateCommandLine(vector<const char*>& commandLine,
                                const char* cmd,
                                const char* numProc,
                                int argStart,
                                int argCount,
                                char* argv[])
{
  if(this->MPIRun.size())
    {
    commandLine.push_back(this->MPIRun.c_str());
    commandLine.push_back(this->MPINumProcessFlag.c_str());
    commandLine.push_back(numProc);

    for(unsigned int i = 0; i < this->MPIPreFlags.size(); ++i)
      {
      commandLine.push_back(this->MPIPreFlags[i].c_str());
      }
    }

  commandLine.push_back(cmd);

  for(unsigned int i = 0; i < this->MPIPostFlags.size(); ++i)
    {
    commandLine.push_back(MPIPostFlags[i].c_str());
    }

  // remaining flags for the test
  for(int ii = argStart; ii < argCount; ++ii)
    {
    commandLine.push_back(argv[ii]);
    }

  commandLine.push_back(0);
}
//----------------------------------------------------------------------------
int H5FDdsmTestDriver::StartServer(H5FDdsm_sysProcess* server, const char* name,
                              vector<char>& out,
                              vector<char>& err)
{
  if(!server)
    {
    return 1;
    }
  cerr << "H5FDdsmTestDriver: starting process " << name << "\n";
  H5FDdsm_sysProcess_SetTimeout(server, this->TimeOut);
  H5FDdsm_sysProcess_Execute(server);
  int foundWaiting = 0;
  string output;
  while(!foundWaiting)
    {
    int pipe = this->WaitForAndPrintLine(name, server, output, 100.0, out, err,
                                         &foundWaiting);
    if(pipe == H5FDdsm_sysProcess_Pipe_None ||
       pipe == H5FDdsm_sysProcess_Pipe_Timeout)
      {
      break;
      }
    }
  if(foundWaiting)
    {
    cerr << "H5FDdsmTestDriver: " << name << " sucessfully started.\n";
    return 1;
    }
  else
    {
    cerr << "H5FDdsmTestDriver: " << name << " never started.\n";
    H5FDdsm_sysProcess_Kill(server);
    return 0;
    }
}
//----------------------------------------------------------------------------
int H5FDdsmTestDriver::StartClient(H5FDdsm_sysProcess* client, const char* name)
{
  if(!client)
    {
    return 1;
    }
  cerr << "H5FDdsmTestDriver: starting process " << name << "\n";
  H5FDdsm_sysProcess_SetTimeout(client, this->TimeOut);
  H5FDdsm_sysProcess_Execute(client);
  if(H5FDdsm_sysProcess_GetState(client) == H5FDdsm_sysProcess_State_Executing)
    {
    cerr << "H5FDdsmTestDriver: " << name << " sucessfully started.\n";
    return 1;
    }
  else
    {
    this->ReportStatus(client, name);
    H5FDdsm_sysProcess_Kill(client);
    return 0;
    }
}
//----------------------------------------------------------------------------
void H5FDdsmTestDriver::Stop(H5FDdsm_sysProcess* p, const char* name)
{
  if(p)
    {
    cerr << "H5FDdsmTestDriver: killing process " << name << "\n";
    H5FDdsm_sysProcess_Kill(p);
    H5FDdsm_sysProcess_WaitForExit(p, 0);
    }
}
//----------------------------------------------------------------------------
int H5FDdsmTestDriver::OutputStringHasError(const char* pname, string& output)
{
  const char* possibleMPIErrors[] = {
    "error",
    "Missing:",
    "core dumped",
    "process in local group is dead",
    "Segmentation fault",
    "erroneous",
    "ERROR:",
    "Error:",
    "mpirun can *only* be used with MPI programs",
    "due to signal",
    "failure",
    "bnormal termination",
    "failed",
    "FAILED",
    "Failed",
    0
  };

  const char* nonErrors[] = {
    "Memcheck, a memory error detector",  //valgrind
    "error in locking authority file",  //Ice-T
    "WARNING: Far depth failed sanity check, resetting.", //Ice-T
    0
  };

  if(this->AllowErrorInOutput)
    {
    return 0;
    }

  vector<string> lines;
  vector<string>::iterator it;
  H5FDdsm_sys::SystemTools::Split(output.c_str(), lines);

  int i, j;

  for ( it = lines.begin(); it != lines.end(); ++ it )
    {
    for(i =0; possibleMPIErrors[i]; ++i)
      {
      if(it->find(possibleMPIErrors[i]) != it->npos)
        {
        int found = 0;
        for (j = 0; nonErrors[j]; ++ j)
          {
          if ( it->find(nonErrors[j]) != it->npos )
            {
            found = 1;
            }
          }
        if ( !found )
          {
          cerr << "H5FDdsmTestDriver: ***** Test will fail, because the string: \""
            << possibleMPIErrors[i]
            << "\"\nH5FDdsmTestDriver: ***** was found in the following output from the "
            << pname << ":\n\""
            << it->c_str() << "\"\n";
          return 1;
          }
        }
      }
    }
  return 0;
}
//----------------------------------------------------------------------------
#define VTK_CLEAN_PROCESSES \
  H5FDdsm_sysProcess_Delete(client); \
  H5FDdsm_sysProcess_Delete(server);
//----------------------------------------------------------------------------
int H5FDdsmTestDriver::Main(int argc, char* argv[])
{
  this->CollectConfiguredOptions();
  if(!this->ProcessCommandLine(argc, argv))
    {
    return 1;
    }

  // mpi code
  // Allocate process managers.
  H5FDdsm_sysProcess* server = 0;
  H5FDdsm_sysProcess* client = 0;
  if(this->TestServer)
    {
    server = H5FDdsm_sysProcess_New();
    if(!server)
      {
      VTK_CLEAN_PROCESSES;
      cerr << "H5FDdsmTestDriver: Cannot allocate H5FDdsm_sysProcess to run the server.\n";
      return 1;
      }
    }
  client = H5FDdsm_sysProcess_New();
  if(!client)
    {
    VTK_CLEAN_PROCESSES;
    cerr << "H5FDdsmTestDriver: Cannot allocate H5FDdsm_sysProcess to run the client.\n";
    return 1;
    }

  vector<char> ClientStdOut;
  vector<char> ClientStdErr;
  vector<char> ServerStdOut;
  vector<char> ServerStdErr;

  vector<const char*> serverCommand;
  if(server)
    {
    const char* serverExe = this->ServerExecutable.c_str();

    this->CreateCommandLine(serverCommand,
                            serverExe,
                            this->MPIServerNumProcessFlag.c_str(),
                            this->ArgStart, argc, argv);
    this->ReportCommand(&serverCommand[0], "server");
    H5FDdsm_sysProcess_SetCommand(server, &serverCommand[0]);
    H5FDdsm_sysProcess_SetWorkingDirectory(server, this->GetDirectory(serverExe).c_str());
    }

  // Construct the client process command line.
  vector<const char*> clientCommand;
  
  const char* clientExe = this->ClientExecutable.c_str();
  this->CreateCommandLine(clientCommand,
                          clientExe,
                          this->MPIServerNumProcessFlag.c_str());
//                          this->ArgStart, argc, argv);
  this->ReportCommand(&clientCommand[0], "client");
  H5FDdsm_sysProcess_SetCommand(client, &clientCommand[0]);
  H5FDdsm_sysProcess_SetWorkingDirectory(client, this->GetDirectory(clientExe).c_str());

  // Start the server if there is one
  if(!this->StartServer(server, "server",
      ServerStdOut, ServerStdErr))
    {
    cerr << "H5FDdsmTestDriver: Server never started.\n";
    VTK_CLEAN_PROCESSES;
    return -1;
    }
  // Now run the client
  if(!this->StartClient(client, "client"))
    {
    this->Stop(server, "server");
    VTK_CLEAN_PROCESSES;
    return -1;
    }

  // Report the output of the processes.
  int clientPipe = 1;

  string output;
  int mpiError = 0;
  while(clientPipe)
    {
    clientPipe = this->WaitForAndPrintLine("client", client, output, 0.1,
                                           ClientStdOut, ClientStdErr, 0);
    if(!mpiError && this->OutputStringHasError("client", output))
      {
      mpiError = 1;
      }
    // If client has died, we wait for output from the server processess
    // for this->ServerExitTimeOut, then we'll kill the servers, if needed.
    double timeout = (clientPipe)? 0.1 : this->ServerExitTimeOut;
    output = "";
    this->WaitForAndPrintLine("server", server, output, timeout,
                              ServerStdOut, ServerStdErr, 0);
    if(!mpiError && this->OutputStringHasError("server", output))
      {
      mpiError = 1;
      }
    output = "";
    }

  // Wait for the client and server to exit.
  H5FDdsm_sysProcess_WaitForExit(client, 0);

  // Once client is finished, the servers
  // must finish quickly. If not, is usually is a sign that
  // the client crashed/exited before it attempted to connect to 
  // the server.
  if(server)
    {
    H5FDdsm_sysProcess_WaitForExit(server, &this->ServerExitTimeOut);
    }

  // Get the results.
  int clientResult = this->ReportStatus(client, "client");
  int serverResult = 0;
  if(server)
    {
    this->ReportStatus(server, "server");
    H5FDdsm_sysProcess_Kill(server);
    }

  // Free process managers.
  VTK_CLEAN_PROCESSES;

  // Report the server return code if it is nonzero.  Otherwise report
  // the client return code.
  if(serverResult)
    {
    return serverResult;
    }

  if(mpiError)
    {
    cerr << "H5FDdsmTestDriver: Error string found in ouput, H5FDdsmTestDriver returning "
         << mpiError << "\n";
    return mpiError;
    }
  // if both servers are fine return the client result
  return clientResult;
}

//----------------------------------------------------------------------------
void H5FDdsmTestDriver::ReportCommand(const char* const* command, const char* name)
{
  cerr << "H5FDdsmTestDriver: " << name << " command is:\n";
  for(const char* const * c = command; *c; ++c)
    {
    cerr << " \"" << *c << "\"";
    }
  cerr << "\n";
}

//----------------------------------------------------------------------------
int H5FDdsmTestDriver::ReportStatus(H5FDdsm_sysProcess* process, const char* name)
{
  int result = 1;
  switch(H5FDdsm_sysProcess_GetState(process))
    {
    case H5FDdsm_sysProcess_State_Starting:
      {
      cerr << "H5FDdsmTestDriver: Never started " << name << " process.\n";
      } break;
    case H5FDdsm_sysProcess_State_Error:
      {
      cerr << "H5FDdsmTestDriver: Error executing " << name << " process: "
           << H5FDdsm_sysProcess_GetErrorString(process)
           << "\n";
      } break;
    case H5FDdsm_sysProcess_State_Exception:
      {
      cerr << "H5FDdsmTestDriver: " << name
                      << " process exited with an exception: ";
      switch(H5FDdsm_sysProcess_GetExitException(process))
        {
        case H5FDdsm_sysProcess_Exception_None:
          {
          cerr << "None";
          } break;
        case H5FDdsm_sysProcess_Exception_Fault:
          {
          cerr << "Segmentation fault";
          } break;
        case H5FDdsm_sysProcess_Exception_Illegal:
          {
          cerr << "Illegal instruction";
          } break;
        case H5FDdsm_sysProcess_Exception_Interrupt:
          {
          cerr << "Interrupted by user";
          } break;
        case H5FDdsm_sysProcess_Exception_Numerical:
          {
          cerr << "Numerical exception";
          } break;
        case H5FDdsm_sysProcess_Exception_Other:
          {
          cerr << "Unknown";
          } break;
        }
      cerr << "\n";
      } break;
    case H5FDdsm_sysProcess_State_Executing:
      {
      cerr << "H5FDdsmTestDriver: Never terminated " << name << " process.\n";
      } break;
    case H5FDdsm_sysProcess_State_Exited:
      {
      result = H5FDdsm_sysProcess_GetExitValue(process);
      cerr << "H5FDdsmTestDriver: " << name << " process exited with code "
                      << result << "\n";
      } break;
    case H5FDdsm_sysProcess_State_Expired:
      {
      cerr << "H5FDdsmTestDriver: killed " << name << " process due to timeout.\n";
      } break;
    case H5FDdsm_sysProcess_State_Killed:
      {
      cerr << "H5FDdsmTestDriver: killed " << name << " process.\n";
      } break;
    }
  return result;
}
//----------------------------------------------------------------------------

int H5FDdsmTestDriver::WaitForLine(H5FDdsm_sysProcess* process, string& line,
                              double timeout,
                              vector<char>& out,
                              vector<char>& err)
{
  line = "";
  vector<char>::iterator outiter = out.begin();
  vector<char>::iterator erriter = err.begin();
  while(1)
    {
    // Check for a newline in stdout.
    for(;outiter != out.end(); ++outiter)
      {
      if((*outiter == '\r') && ((outiter+1) == out.end()))
        {
        break;
        }
      else if(*outiter == '\n' || *outiter == '\0')
        {
        int length = outiter-out.begin();
        if(length > 1 && *(outiter-1) == '\r')
          {
          --length;
          }
        if(length > 0)
          {
          line.append(&out[0], length);
          }
        out.erase(out.begin(), outiter+1);
        return H5FDdsm_sysProcess_Pipe_STDOUT;
        }
      }

    // Check for a newline in stderr.
    for(;erriter != err.end(); ++erriter)
      {
      if((*erriter == '\r') && ((erriter+1) == err.end()))
        {
        break;
        }
      else if(*erriter == '\n' || *erriter == '\0')
        {
        int length = erriter-err.begin();
        if(length > 1 && *(erriter-1) == '\r')
          {
          --length;
          }
        if(length > 0)
          {
          line.append(&err[0], length);
          }
        err.erase(err.begin(), erriter+1);
        return H5FDdsm_sysProcess_Pipe_STDERR;
        }
      }

    // No newlines found.  Wait for more data from the process.
    int length;
    char* data;
    int pipe = H5FDdsm_sysProcess_WaitForData(process, &data, &length, &timeout);
    if(pipe == H5FDdsm_sysProcess_Pipe_Timeout)
      {
      // Timeout has been exceeded.
      return pipe;
      }
    else if(pipe == H5FDdsm_sysProcess_Pipe_STDOUT)
      {
      // Append to the stdout buffer.
      vector<char>::size_type size = out.size();
      out.insert(out.end(), data, data+length);
      outiter = out.begin()+size;
      }
    else if(pipe == H5FDdsm_sysProcess_Pipe_STDERR)
      {
      // Append to the stderr buffer.
      vector<char>::size_type size = err.size();
      err.insert(err.end(), data, data+length);
      erriter = err.begin()+size;
      }
    else if(pipe == H5FDdsm_sysProcess_Pipe_None)
      {
      // Both stdout and stderr pipes have broken.  Return leftover data.
      if(!out.empty())
        {
        line.append(&out[0], outiter-out.begin());
        out.erase(out.begin(), out.end());
        return H5FDdsm_sysProcess_Pipe_STDOUT;
        }
      else if(!err.empty())
        {
        line.append(&err[0], erriter-err.begin());
        err.erase(err.begin(), err.end());
        return H5FDdsm_sysProcess_Pipe_STDERR;
        }
      else
        {
        return H5FDdsm_sysProcess_Pipe_None;
        }
      }
    }
}
//----------------------------------------------------------------------------
void H5FDdsmTestDriver::PrintLine(const char* pname, const char* line)
{
  // if the name changed then the line is output from a different process
  if(this->CurrentPrintLineName != pname)
    {
    cerr << "-------------- " << pname
         << " output --------------\n";
    // save the current pname
    this->CurrentPrintLineName = pname;
    }
  cerr << line << "\n";
  cerr.flush();
}
//----------------------------------------------------------------------------
int H5FDdsmTestDriver::WaitForAndPrintLine(const char* pname, H5FDdsm_sysProcess* process,
                                      string& line, double timeout,
                                      vector<char>& out,
                                      vector<char>& err,
                                      int* foundWaiting)
{
  int pipe = this->WaitForLine(process, line, timeout, out, err);
  if(pipe == H5FDdsm_sysProcess_Pipe_STDOUT || pipe == H5FDdsm_sysProcess_Pipe_STDERR)
    {
    this->PrintLine(pname, line.c_str());
    if(foundWaiting && (line.find("Waiting") != line.npos))
      {
      *foundWaiting = 1;
      }
    }
  return pipe;
}
//----------------------------------------------------------------------------
string H5FDdsmTestDriver::GetDirectory(string location)
{
  return H5FDdsm_sys::SystemTools::GetParentDirectory(location.c_str());
}
