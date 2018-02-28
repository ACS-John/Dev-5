00010 ! Replace S:\acsGL\PRClose
00020 ! GENERAL LEDGER Payroll Only Month End Closing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   fntop(program$,"Payroll Only Month End Closing")
00080 ! fnwait - "GENERAL LEDGER Payroll Only Month End Closing IN PROCESS"
00090 ! empty the General Ledger Payroll Checks File !:
        open #20: "Name=[Q]\GLmstr\ACPRCKS.H[cno],Size=0,RecL=110,Replace",internal,output: close #20: 
00100   open #prmstr=1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRINDEX.h[cno],NoShr",internal,outIn,keyed 
00110 READ_PRMSTR: ! 
00120   read #prmstr,using 'Form POS 271,2*N 5': n1,n2 eof DONE
00130   rewrite #prmstr,using 'Form POS 271,2*N 5': 0,0
00140   goto READ_PRMSTR
00150 ! ______________________________________________________________________
00160 DONE: ! 
00170   close #prmstr: 
00180   goto XIT
00190 ! ______________________________________________________________________
00200 XIT: fnxit
00210 ! ______________________________________________________________________
00220 ! <Updateable Region: ERTN>
00230 ERTN: fnerror(program$,err,line,act$,"NO")
00240   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00250   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00260   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00270 ERTN_EXEC_ACT: execute act$ : goto ERTN
00280 ! /region
00290 ! ______________________________________________________________________
