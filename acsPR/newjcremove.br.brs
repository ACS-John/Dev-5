00010 ! Replace S:\acsPR\newjcRemove
00020 ! Remove Job Cost Payroll Jobs
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fncno,fnerror,fnxit,fntop,fntos,fnlbl,fncmbjob,fncmdkey,fnacs,fnmsgbox,fncombof
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),eno$*12,jno$*6
00080   dim contact$*30,ph$*12,email$*60
00090   dim tr(9),pd$*30,tn$*6,n$*40,cap$*128,ml$(1)*70
00100   dim resp$(1)*60
00110 ! ______________________________________________________________________
00120   fntop("S:\acsPR\newjcRemove",cap$="Remove Completed Jobs")
00130   fncno(cno)
00140 ! 
00150   open #1: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno),internal,outin: close #1: 
00160 ! ______________________________________________________________________
00170   execute "Copy "&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&" JCMSTR.X -n"
00180   execute "Copy "&env$('Q')&"\PRmstr\JCTRANS.h"&str$(cno)&" JCTRANS.X -n"
00190   execute "Copy "&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&" JCCAT.X -n"
00200 ! ______________________________________________________________________
00210   open #1: "Name=JCMSTR.X,KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno),internal,outin,keyed 
00220 ! ______________________________________________________________________
00230 ASKJOB: ! 
00240   fntos(sn$="jccpr1J") !:
        let respc=0
00250   fnlbl(1,1,"Job #:",8,1)
00260   fncmbjob(1,11) !:
        let resp$(respc+=1)=jn$
00270   if trim$(jn$)<>"" then let fnlbl(3,1,"Last job processed:"&trim$(jn$),35,1)
00280   fncmdkey("&Next",1,1,0,"Process the job" ) !:
        fncmdkey("Com&plete",2,0,0,"Finished with all jobs.") !:
        fncmdkey("&Cancel",5,0,1,"Cancel without deleting any jobs.")
00290   fnacs(sn$,0,mat resp$,ck)
00300   if ck=5 then goto XIT
00310   if ck=2 then goto DELETE_THEM
00320   let jn$=lpad$(trim$(resp$(1)(1:6)),6)
00330   mat ml$(1) !:
        let ml$(1)="Do you really want to delete job # "&jn$ !:
        fnmsgbox(mat ml$,resp$,cap$,36)
00340   if resp$="Yes" then goto L350 else goto ASKJOB
00350 L350: rewrite #1,using 'Form POS 157,N 2',key=jn$: 9 nokey ASKJOB
00360   goto ASKJOB
00370 ! ______________________________________________________________________
00380 DELETE_THEM: ! 
00390   restore #1: 
00400   open #2: "Name=JCCAT.X,KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno),internal,input,keyed 
00410   open #3: "Name=JCTRANS.X",internal,input,relative 
00420   open #11: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno),internal,output 
00430   close #11,free: 
00440   open #11: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",SIZE=0,RecL=300",internal,output 
00450   open #12: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno),internal,output 
00460   close #12,free: 
00470   open #12: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",SIZE=0,RecL=123",internal,output 
00480   open #13: "Name="&env$('Q')&"\PRmstr\JCTRANS.h"&str$(cno),internal,output 
00490   close #13,free: 
00500   open #13: "Name="&env$('Q')&"\PRmstr\JCTRANS.h"&str$(cno)&",SIZE=0,RecL=88",internal,outin,relative 
00510   let ot4=1
00520   write #13,using L530,rec=1: " ","",mat tr," ",ot4
00530 L530: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
00540 L540: read #1,using "Form POS 1,C 6,C 40,3*C 30,N 6,2*PD 7.2,N 2,C 30,C 12,C 60": jn$,n$,mat a$,mat b,contact$,ph$,email$ eof EOF1
00550   form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
00560   if b(4)=9 then goto L540
00570   cn$=jn$&"     "
00580   read #2,using L590,key>=cn$: cn$,k$,mat l,mat ta nokey L540
00590 L590: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
00600   goto L620
00610 L610: read #2,using L590: cn$,k$,mat l,mat ta eof L790
00620 L620: if jn$><cn$(1:6) then goto L790
00630   if ta(1)=0 then goto L770
00640   adr=ta(1)
00650   mat ta=(0)
00660 L660: read #3,using L530,rec=adr: eno$,jno$,mat tr,pd$,nta
00670   let ot4=ot4+1
00680   if nta>0 then let ota=ot4+1 else let ota=0
00690   write #13,using L530,rec=ot4: eno$,jno$,mat tr,pd$,ota
00700   rewrite #13,using L710,rec=1: ot4
00710 L710: form pos 86,pd 3
00720   if ta(1)=0 then let ta(1)=ot4
00730   let ta(2)=ot4
00740   if nta=0 then goto L770
00750   adr=nta
00760   goto L660
00770 L770: write #12,using L590: cn$,k$,mat l,mat ta
00780   goto L610
00790 L790: write #11,using "Form POS 1,C 6,C 40,3*C 30,N 6,2*PD 7.2,N 2,C 30,C 12,C 60": jn$,n$,mat a$,mat b,contact$,ph$,email$
00800   goto L540
00810 ! ______________________________________________________________________
00820 EOF1: close #1,free: 
00830   close #2,free: 
00840   close #3,free: 
00850   close #11: 
00860   close #12: 
00870   close #13: 
00880   execute "Index "&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&","&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",1,6,Replace,DupKeys -n"
00890   execute "Index "&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&","&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",1,11,Replace,DupKeys -n"
00895   let df$=env$('Q')&"\PRmstr\jcmstr.h"&str$(cno) : let if$=env$('Q')&"\PRmstr\jcindx.h"&str$(cno) !:
        fncombof("CJob.h"&str$(cno),lyne,mypos,43,df$,1,6,7,25,if$,1)
00900   goto XIT
00910 ! ______________________________________________________________________
00920 XIT: let fnxit
00930 ! ______________________________________________________________________
00940 ! <Updateable Region: ERTN>
00950 ERTN: let fnerror(program$,err,line,act$,"xit")
00960   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00970   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00980   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00990 ERTN_EXEC_ACT: execute act$ : goto ERTN
01000 ! /region
01010 ! ______________________________________________________________________
