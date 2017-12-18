00010 ! Replace S:\acsPR\jcRemove
00020 ! Remove Job Cost Payroll Jobs
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnoldmsgbox,fnopenwin,fncno,fnerror,fnxit,fntop,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),eno$*12,jno$*6
00080   dim tr(9),pd$*30,tn$*6,n$*40,cap$*128,message$*40,msgline$(2)*60
00090   dim response$(5)*1
00100 ! ______________________________________________________________________
00110   fntop("S:\acsPR\jcRemove",cap$="Remove Completed Jobs")
00120   fncno(cno)
00130 ! 
00135   fnconsole(1)
00140   open #1: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&env$('cno'),internal,outin: close #1: 
00150 ! ______________________________________________________________________
00160   execute "Copy "&env$('Q')&"\PRmstr\JCMSTR.h"&env$('cno')&" JCMSTR.X -n"
00170   execute "Copy "&env$('Q')&"\PRmstr\JCTRANS.h"&env$('cno')&" JCTRANS.X -n"
00180   execute "Copy "&env$('Q')&"\PRmstr\JCCAT.H"&env$('cno')&" JCCAT.X -n"
00190 ! ______________________________________________________________________
00200   open #1: "Name=JCMSTR.X,KFName="&env$('Q')&"\PRmstr\JCIndx.h"&env$('cno'),internal,outin,keyed 
00210 ! ______________________________________________________________________
00220 L220: pr newpage
00230   fnopenwin(win=101,10,20,14,59,cap$)
00240   pr #101,fields "4,2,C 21,N": "Job Number to Remove:"
00250   pr f "15,35,C 09,B,5": "Done (F5)"
00260 L260: input #101,fields "4,24,C 6,UT,N": jn$
00270   jn$=lpad$(rtrm$(jn$),6)
00280   if cmdkey=5 then goto L390
00290   if ltrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto L260
00300   read #1,using L310,key=jn$: n$ nokey L220
00310 L310: form pos 7,c 40
00320   msgline$(1)="Are you sure you wish to delete Job Number "&ltrm$(jn$)
00330   msgline$(2)=n$
00340   fnoldmsgbox(mat response$,cap$, mat msgline$,2)
00350   if response$(1)="Y" then goto L360 else goto L220
00360 L360: rewrite #1,using 'Form POS 157,N 2',key=jn$: 9 nokey L220
00370   goto L220
00380 ! ______________________________________________________________________
00390 L390: pr newpage
00400   message$="Removeing completed Jobs..."
00410   fnwait(message$,0)
00420   restore #1: 
00430   open #2: "Name=JCCAT.X,KFName="&env$('Q')&"\PRmstr\CatIndx.h"&env$('cno'),internal,input,keyed 
00440   open #3: "Name=JCTRANS.X",internal,input,relative 
00450   open #11: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&env$('cno'),internal,output 
00460   close #11,free: 
00470   open #11: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&env$('cno')&",SIZE=0,RecL=300",internal,output 
00480   open #12: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&env$('cno'),internal,output 
00490   close #12,free: 
00500   open #12: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&env$('cno')&",SIZE=0,RecL=123",internal,output 
00510   open #13: "Name="&env$('Q')&"\PRmstr\JCTRANS.h"&env$('cno'),internal,output 
00520   close #13,free: 
00530   open #13: "Name="&env$('Q')&"\PRmstr\JCTRANS.h"&env$('cno')&",SIZE=0,RecL=88",internal,outin,relative 
00540   ot4=1
00550   write #13,using L560,rec=1: " ","",mat tr," ",ot4
00560 L560: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
00570 L570: read #1,using L580: jn$,n$,mat a$,mat b eof EOF1
00580 L580: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
00590   if b(4)=9 then goto L570
00600   cn$=jn$&"     "
00610   read #2,using L620,key>=cn$: cn$,k$,mat l,mat ta nokey L570
00620 L620: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
00630   goto L650
00640 L640: read #2,using L620: cn$,k$,mat l,mat ta eof L820
00650 L650: if jn$><cn$(1:6) then goto L820
00660   if ta(1)=0 then goto L800
00670   adr=ta(1)
00680   mat ta=(0)
00690 L690: read #3,using L560,rec=adr: eno$,jno$,mat tr,pd$,nta
00700   ot4=ot4+1
00710   if nta>0 then ota=ot4+1 else ota=0
00720   write #13,using L560,rec=ot4: eno$,jno$,mat tr,pd$,ota
00730   rewrite #13,using L740,rec=1: ot4
00740 L740: form pos 86,pd 3
00750   if ta(1)=0 then ta(1)=ot4
00760   ta(2)=ot4
00770   if nta=0 then goto L800
00780   adr=nta
00790   goto L690
00800 L800: write #12,using L620: cn$,k$,mat l,mat ta
00810   goto L640
00820 L820: write #11,using L580: jn$,n$,mat a$,mat b
00830   goto L570
00840 ! ______________________________________________________________________
00850 EOF1: close #1,free: 
00860   close #2,free: 
00870   close #3,free: 
00880   close #11: 
00890   close #12: 
00900   close #13: 
00910   execute "Index "&env$('Q')&"\PRmstr\JCMSTR.h"&env$('cno')&","&env$('Q')&"\PRmstr\JCIndx.h"&env$('cno')&",1,6,Replace,DupKeys -n"
00920   execute "Index "&env$('Q')&"\PRmstr\JCCAT.H"&env$('cno')&","&env$('Q')&"\PRmstr\CatIndx.h"&env$('cno')&",1,11,Replace,DupKeys -n"
00930   goto XIT
00940 ! ______________________________________________________________________
00950 XIT: fnxit
00960 ! ______________________________________________________________________
00970 ! <Updateable Region: ERTN>
00980 ERTN: fnerror(program$,err,line,act$,"xit")
00990   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01000   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01010   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01020 ERTN_EXEC_ACT: execute act$ : goto ERTN
01030 ! /region
01040 ! ______________________________________________________________________
