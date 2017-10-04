00020 ! 
00030   on error goto L700
00040   dim gl$(10)*12,ga(10),pgl$*12
00050   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fngethandle
00060   fntop(program$,cap$="AR Merge")
00070   fncno(cno,cnam$)
00080   dim p$*5,iv$*12,tr(6),id$*20,sc1$(5),sc2$(9),hd$(2)*50,ta(2)
00090   pr newpage
00100   pr f "10,10,c 50,H,N": "A/R Merge Transactions In Process"
00110   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L700
00120   open #2: "Name="&env$('Q')&"\TMmstr\ARTrans.h"&str$(cno)&",Shr",internal,outin,relative ioerr L700
00130   open #h_addr:=fngethandle: "Name="&env$('Temp')&"\Addr."&session$,internal,outin,relative ioerr L700
00140 ! open #h_armotran:=4: "Name="&env$('Q')&"\TMmstr\ARMoTran.h"&str$(cno)&",Shr",internal,output ioerr L700
00150 LOOP_TOP: ! 
00152   let r3=r3+1
00160   read #h_addr,using F_ADDR,rec=r3: p$,iv$,mat tr,id$,pgl$,gl$(1),ga(1),gl$(2),ga(2),gl$(3),ga(3),gl$(4),ga(4),gl$(5),ga(5),gl$(6),ga(6),gl$(7),ga(7),gl$(8),ga(8),gl$(9),ga(9),gl$(10),ga(10) eof L540,norec L540 ioerr L700
00170 F_ADDR: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,c 12,10*(c 12,pd 5.2)
00180   if tr(6)=9 then goto LOOP_TOP
00190   if ltrm$(p$)="-1" then goto L220
00200   read #1,using L210,key=p$: am6,mat ta nokey CLSMSTR_NOKEY ioerr L700
00210 L210: form pos 283,pd 5.2,pos 299,2*pd 3
00220 L220: let iv$=lpad$(rtrm$(iv$),12)
00230   if tr(5)><1 then goto L240
00240 L240: if tr(5)<3 or tr(5)=5 then am6=am6+tr(3) else am6=am6-tr(3)
00250   if tr(5)=3 then am6=am6-tr(2)
00260   let tr2=tr(2)
00270   if tr(5)=3 then let tr(3)=tr(3)+tr2
00280   let tr(2)=tr(3)
00290   if tr(5)=4 then let tr(5)=6
00300   if tr(5)=3 then let tr(5)=4
00310   if tr(5)=2 then let tr(5)=5
00320   if ltrm$(p$)="-1" then goto L420
00330   read #2,using L370,rec=1,reserve: rec2
00340 L340: let rec2=lrec(2)+1
00350   write #2,using L360,rec=rec2,reserve: p$,iv$,mat tr,id$,nta duprec L340
00360 L360: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
00370 L370: form pos 58,pd 3
00380   if ta(2)=0 then let ta(1)=rec2 else rewrite #2,using L370,rec=ta(2),reserve: rec2
00390   let ta(2)=rec2
00400   rewrite #2,using L370,rec=1,release: rec2
00410   rewrite #1,using L210,key=p$: am6,mat ta nokey CLSMSTR_NOKEY
00420 L420: if tr(5)=4 then let tr(2)=tr2
00430   if tr(5)=4 then let tr(3)=tr(3)-tr2
00440 ! write #h_armotran,using L450: p$,iv$,mat tr,id$,pgl$,mat gl$,mat ga
00450 ! L450: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,11*c 12,10*pd 5.2
00460   rewrite #h_addr,using L470,rec=r3: 9
00470 L470: form pos 37,n 1
00480   goto LOOP_TOP
00490 CLSMSTR_NOKEY: if rtrm$(p$)="" or rtrm$(ltrm$(p$))="0" then goto LOOP_TOP
00500   let t7=9
00510   pr #255: ,"CANNOT LOCATE ACCOUNT # ";p$
00520   let prtcode=1
00530   goto LOOP_TOP
00540 L540: close #1: 
00550   close #2: 
00560   close #h_addr: 
00570 ! close #h_armotran: 
00580   if prtcode=0 then goto L600
00590   if nw=1 then close #255: else pr #255: newpage
00600 L600: if t7=9 then goto L620
00610 XIT: let fnxit
00620 L620: pr newpage,"BE SURE TO SET UP THE A/R ACCOUNTS AS"
00630   pr "INDICATED ON THE PRINT-OUT.  THEN REENTER ANY "
00640   pr "TRANSACTIONS THAT WERE REJECTED."
00650   pr 
00660   pr 
00670   pr f "22,2,c 40": "Press enter to return to system menu."
00680   input fields "23,2,c 1,ae,n": pause$
00690   goto XIT
00700 L700: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L720
00710   goto L760
00720 L720: pr newpage
00730   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L750
00740   goto L760
00750 L750: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00760 L760: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00770   input fields "24,60,C 1,N": quitcode$
00780   if rtrm$(uprc$(quitcode$))="Q" then goto L820
00790   pr f "23,3,C 78,N": ""
00800   pr f "24,3,C 78,N": ""
00810   retry 
00820 L820: goto XIT
