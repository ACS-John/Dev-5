00010 ! Replace S:\acsPR\newprAuto
00020 ! automatic processing - chain program
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit, fnxit,fncno,fnerror,fnputcno,fnprocess,fnprg,fnchain,fnps,fnpgnum,fnkillauto,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim prg$*20,a$*40,cnam$*40,ml$(3)*80
00080 ! ______________________________________________________________________
00090   fncno(cno)
00100 L100: if fnprocess=0 then goto XIT
00110 ! ______________________________________________________________________
00120   pgnum=fnpgnum
00130   if pgnum=0 then open #prclnt=1: "Name="&env$('Q')&"\PRmstr\PrClnt.dat,NoShr",internal,outin,relative  !:
          read #prclnt,using 'Form POS 46,3*N 1',rec=1: w,m,q !:
          close #prclnt: 
00140 L140: open #20: "Name="&env$('Q')&"\PRmstr\newPrPGMN.H"&env$('cno')&",Shr",internal,input,relative ioerr MSGBOX1 !:
        read #20,using 'Form POS 1,C 20,X 35,3*N 1',rec=pgnum+=1: prg$,wk,mo,qt eof XIT,norec XIT !:
        close #20: 
00150   if w=1 and wk<>1 then goto L140 ! WEEKLY PERIOD NOT SELECTED
00160   if m=1 and mo<>1 then goto L140 ! MONTHY PERIOD NOT SELECTED
00170   if q=1 and qt<>1 then goto L140 ! QUARTER PERIOD NOT SELECTED
00180   if rtrm$(prg$)="" then goto L260
00190   fnprg(prg$,put=2)
00200   fnpgnum(pgnum) : fnps(ps)
00210   goto CHAIN_PRG
00220 MSGBOX1: ! 
00230   mat ml$(3) !:
        ml$(1)="The order for automatic processing has" !:
        ml$(2)="never been set for company # "&env$('cno')&"." !:
        ml$(3)="Click OK to skip this company." !:
        fnmsgbox(mat ml$,resp$,cap$,49)
00240   goto L260
00250 ! ______________________________________________________________________
00260 L260: fnkillauto : fnpgnum(-1) !:
        ! ! CHECK FOR ADDITIONAL COMPANIES
00270   open #prclnt=1: "Name="&env$('Q')&"\PRmstr\PrClnt.dat,NoShr",internal,outin,relative ioerr XIT
00280   for j=2 to 20
00290     read #prclnt,using 'Form POS 1,N 5,POS 46,3*N 1',rec=j: cno,w,m,q
00300     if cno<>0 then goto L340
00310   next j
00320   goto XIT
00330 ! ______________________________________________________________________
00340 L340: fnputcno(cno) : fnprocess(process=1)
00350   rewrite #prclnt,using 'Form POS 1,N 5,C 40,3*N 1',rec=j: 0," ",0,0,0
00360   close #prclnt: 
00370   goto L100
00380 ! ______________________________________________________________________
00390 XIT: ! 
00400   execute "Free AutoPrn."&wsid$&" -n" ioerr L410
00410 L410: fnxit
00420 CHAIN_PRG: fnchain(prg$)
00430 ! ______________________________________________________________________
00440 ! <Updateable Region: ERTN>
00450 ERTN: fnerror(program$,err,line,act$,"xit")
00460   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00470   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00480   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00490 ERTN_EXEC_ACT: execute act$ : goto ERTN
00500 ! /region
