type
  CXTargetInfoImpl* = object
  CXTranslationUnitImpl* = object
  CXVirtualFileOverlayImpl* = object
  CXModuleMapDescriptorImpl* = object
  time_t* = clong
# typedef long int __time_t;



  # CXErrorCode* {.pure, size: sizeof(cint).} = enum
  #   CXError_Success = 0,
  #   CXError_Failure = 1,
  #   CXError_Crashed = 2,
  #   CXError_InvalidArguments = 3,
  #   CXError_ASTReadError = 4
