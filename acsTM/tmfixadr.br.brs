00020   dim ca(10),ta(25,2),fb(25),k$*5,e$*9,b(8),sc$*4,iv$*12,fl5$*40,id$*30
00022   on error goto L690
00030   dim exec$*132
00040   library 'S:\Core\Library': fncno
00080   let fncno(cno)
00090   pr newpage
00100   pr fields "10,15,c 60,h,n": "TM FIX TRANSACTION ADDRESSES IN PROCESS"
00110   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.H"&str$(cno)&",NoShr",internal,outin,keyed ioerr L690
00120   open #2: "Name="&env$('Q')&"\TMmstr\TMTrans.h"&str$(cno)&",NoShr",internal,input,relative ioerr L690
00130   open #4: "Name="&env$('Q')&"\TMmstr\WORK1.h"&str$(cno),internal,output ioerr L150
00140   close #4,free: 
00150 L150: open #4: "Name="&env$('Q')&"\TMmstr\WORK1.H"&str$(cno)&",Replace,RecL=86",internal,outin,relative ioerr L690
00160   write #4,using L170,rec=1: k$,e$,mat b,sc$,iv$,1,id$
00170 L170: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30
00180   let r4=1
00190   let fl5$="Name="&env$('Q')&"\TMmstr\TMTRAddr.h"&str$(cno)
00200   let exec$="FREE "&fl5$(6:len(fl5$))
00210   execute exec$
00220   open #5: fl5$&",Replace,RecL=175",internal,outin,relative ioerr L690
00230   write #5,using L240,rec=1: mat ta,mat fb
00240 L240: form pos 1,50*pd 3,25*n 1
00250   let r5=1
00260   rewrite #5,using L270,rec=r5: r5
00270 L270: form pos 1,pd 3
00280 L280: form pos 230,10*pd 3
00290 L290: let r2=r2+1
00300   read #2,using L170,rec=r2: k$,e$,mat b,sc$,iv$,nta,id$ norec L620,eof L620 ioerr L690
00310   if b(4)=0 or b(5)<1 or b(5)>25 then goto L290
00320   read #1,using L280,key=k$: mat ca nokey L290 ioerr L690
00330   let r4=r4+1
00340   write #4,using L170,rec=r4: k$,e$,mat b,sc$,iv$,0,id$
00350   rewrite #4,using L360,rec=1: r4
00360 L360: form pos 54,pd 3
00370   if b(5)>10 then goto L290
00380   if b(8)=0 then b8=25 else b8=b(8)
00390   if ca(b(5))=0 then goto L490
00400   let p1=1+(b8-1)*6
00410   let p2=150+b8
00420   read #5,using L430,rec=ca(b(5)): ta1,ta2,fb1 norec L490 ioerr L690
00430 L430: form pos p1,2*pd 3,pos p2,n 1
00440   if ta2<>0 then rewrite #4,using L360,rec=ta2: r4 else let ta1=lta
00442   if b(7)>0 then goto L460
00450   if fb1<2 then let fb1=abs(b(7))
00460 L460: if ta1=0 then let ta1=r4
00470   rewrite #5,using L430,rec=ca(b(5)): ta1,r4,fb1
00480   goto L290
00490 L490: let r5=r5+1
00500   mat ta=(0)
00510   mat fb=(0)
00520   ca(b(5))=r5
00530   let ta(b8,1)=r4
00540   let ta(b8,2)=r4
00550   if b(7)=-2 then let fb(b8)=2
00560   if fb(b8)=2 then goto L580
00570   if b(7)=-1 then let fb(b8)=1
00580 L580: write #5,using L240,rec=r5: mat ta,mat fb
00590   rewrite #5,using L270,rec=1: r5
00600   rewrite #1,using L280,key=k$: mat ca
00610   goto L290
00620 L620: close #1: 
00630   close #2,free: 
00640   close #4: 
00650   close #5: 
00660   let exec$="Rename "&env$('Q')&"\TMmstr\WORK1.H"&str$(cno)&","&env$('Q')&"\TMmstr\TMTrans.h"&str$(cno)
00670   execute exec$
00680   chain "S:\Time Management\Client Legacy"
00690 L690: if err=61 then pr fields "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L710
00700   goto L750
00710 L710: pr newpage
00720   if err=4148 then pr fields "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L740
00730   goto L750
00740 L740: pr fields "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00750 L750: pr fields "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00760   input fields "24,60,C 1,N": quitcode$
00770   if rtrm$(uprc$(quitcode$))="Q" then goto L810
00780   pr fields "23,3,C 78,N": ""
00790   pr fields "24,3,C 78,N": ""
00800   retry 
00810 L810: chain "S:\Core\Menu"
