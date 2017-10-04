00010 ! Replace Test\MultiLine
00020 ! -------------------------------------------------------------------
00030   library 'S:\Core\Library': fnprg,fntos,fnacs,fncmdkey,fnmultiline
00040   execute "Config Console Off"
00050   dim resp$(5)*400
00060 ! -------------------------------------------------------------------
00065   pr border: "Test MultiLine"
00070   let prg$='test': let fnprg(prg$,2)
00080   fntos('test')
00090 ! -------------------------------------------------------------------
00100   fnmultiline(2,5,10,30) !:
        let resp$(1)="This is a test"
00110   fncmdkey("This is a test of dynamic button width",5,0,1) !:
        fncmdkey("I",1)
00120   fnacs('test',win,mat resp$,ckey)
00130   pr mat resp$
