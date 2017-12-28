00010 ! Replace S:\acsGL\gljcMrge
00020 ! GENERAL LEDGER JOB COST MERGE Charges; attempts to merge job cost !:
        ! and then chains to regular general ledger merge program.
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno, fnchain
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim rn$*12,jn$*6,ji2(3),cn$*11,l(13),ta(2),tr(9),empnum$*12,empnam$*30
00080 ! ______________________________________________________________________
00090   fntop(program$,"Post Transactions")
00100   fncno(cno)
00110   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative: read #1,using 'Form POS 382,N 2',rec=1: jccode !:
        close #1: 
00120   if jccode<>1 then goto L460
00130   pr newpage
00140   pr f "10,15,Cc 60,N": "GENERAL LEDGER JOB COST MERGE CHARGES IN PROCESS"
00150   open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&env$('cno')&",Shr",internal,outIn,keyed 
00160   open #3: "Name="&env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&env$('cno'),internal,input 
00170   open #5: "Name="&env$('Q')&"\PRmstr\JCTRANS.h"&env$('cno')&",Shr",internal,outIn,relative 
00180 L180: read #3,using L190: dat,ji2(3),postc,rn$,empnam$,jn$,ji2(1),ji2(2) eof L430,ioerr L430
00190 L190: form pos 13,n 6,pd 6.2,pos 27,n 2,c 12,c 30,pos 79,c 6,n 5,n 3
00200   if postc=9 or rn$="999999999999" then goto L180
00210   if ltrm$(rtrm$(rn$))="-1" or ji2(3)=0 then goto L180
00220   jn$=lpad$(rtrm$(jn$),6)
00230   cn$=jn$&lpad$(str$(ji2(1)),5)
00240   read #2,using L250,key=cn$: mat l,mat ta nokey L180
00250 L250: form pos 37,11*pd 5.2,2*pd 2,2*pd 3
00260   nc1=0
00270   l(6)=l(6)+ji2(3)
00280   l(9)=l(9)+ji2(3)
00290   goto L310
00300   nc1=1
00310 L310: read #5,using L320,rec=1,reserve: ot5
00320 L320: form pos 86,pd 3
00330   empnum$=lpad$(rtrm$(rn$),12)
00340 L340: ot5=lrec(5)+1
00350   write #5,using L360,rec=ot5,reserve: empnum$,jn$,ji2(1),ji2(2),0,dat,0,0,0,0,ji2(3),empnam$,0 duprec L340
00360 L360: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
00370   if ta(2)=0 then ta(1)=ot5 else rewrite #5,using L320,rec=ta(2): ot5
00380   rewrite #5,using L320,rec=1,release: ot5
00390   ta(2)=ot5
00400   if nc1=0 then rewrite #2,using L250,key=cn$: mat l,mat ta
00410   goto L180
00420 ! ______________________________________________________________________
00430 L430: close #2: 
00440   close #3: 
00450   close #5: 
00460 L460: fnchain("S:\acsGL\ACGLMRGE")
00470 ! ______________________________________________________________________
00480 XIT: fnxit
00490 ! ______________________________________________________________________
00500 ! <Updateable Region: ERTN>
00510 ERTN: fnerror(program$,err,line,act$,"xit")
00520   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00530   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00540   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00550 ERTN_EXEC_ACT: execute act$ : goto ERTN
00560 ! /region
