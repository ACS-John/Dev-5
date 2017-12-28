00010 ! Replace Test\MultiLine
00020 ! -------------------------------------------------------------------
00030   library 'S:\Core\Library': fnprg,fnTos,fnAcs,fnCmdKey,fnmultiline
00040   execute "Config Console Off"
00050   dim resp$(5)*400
00060 ! -------------------------------------------------------------------
00065   pr border: "Test MultiLine"
00070   prg$='test': fnprg(prg$,2)
00080   fnTos('test')
00090 ! -------------------------------------------------------------------
00100   fnmultiline(2,5,10,30) !:
        resp$(1)="This is a test"
00110   fnCmdKey("This is a test of dynamic button width",5,0,1) !:
        fnCmdKey("I",1)
00120   fnAcs('test',win,mat resp$,ckey)
00130   pr mat resp$
