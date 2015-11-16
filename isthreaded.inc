//x86-64 GTK2 crashes with progress bars
//GTK2 crashes with threading


{$IFDEF LCLgtk2}
  {$ifndef cpux86_64}
    {$DEFINE SHOWPROG}//SHOWPROG =ShowProgressBar
   {$ENDIF}
{$ELSE}
  {$DEFINE SHOWPROG}//SHOWPROG =ShowProgressBar
{$ENDIF}
{$IFDEF UNIX} //Windows is always threaded
         {$IFDEF LCLCocoa}   //for Carbon or Cocoa
               {$DEFINE NoThreads}//NoThreads - single threaded execution
        {$ENDIF}

  {$IFDEF LCLgtk2}
    {$DEFINE NoThreads}//NoThreads - single threaded execution
    //GTK2 does not allow threading
  {$ENDIF}
{$ENDIF}

