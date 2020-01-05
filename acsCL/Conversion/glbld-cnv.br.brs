00010 ! Replace S:\acsCL\Conversion\GLBld-Cnv
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno
00040   on error goto Ertn
00050 ! ______________________________________________________________________
00060   dim gl$*12,de$*50
00070 ! ______________________________________________________________________
00080   fncno(cno)
00090 ! 
00100   open #2: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr",internal,input,keyed 
00110   open #1: "Name=[Q]\CLmstr\GLmstr.H[cno],Size=0,RecL=62,Replace",internal,output 
00120 READ_GLMSTR: ! 
00130   read #2,using 'Form POS 1,C 12,C 50': gl$,de$ eof END1
00140   write #1,using 'Form POS 1,C 12,C 50': gl$,de$
00150   goto READ_GLMSTR
00160 ! ______________________________________________________________________
00170 END1: close #1: 
00180   close #2: 
00190   execute "Index [Q]\CLmstr\GLmstr.H[cno]"&' '&"[Q]\CLmstr\GLINDEX.h[cno] 1 12 Replace DupKeys -n"
00200   goto XIT
00210 ! ______________________________________________________________________
00220 XIT: fnxit
00230 ! ______________________________________________________________________
00240 ! <Updateable Region: ERTN>
00250 ERTN: fnerror(program$,err,line,act$,"xit")
00260   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00270   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00280   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00290 ERTN_EXEC_ACT: execute act$ : goto ERTN
00300 ! /region
00310 ! ______________________________________________________________________
