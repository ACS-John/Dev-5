00010 ! Replace S:\acsPR\jcNamLst
00020 ! Print Name and Number List
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fncloseprn,fnopenprn,fncno,fnerror,fndat,fnprocess,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim dat$*20,jn$*6,n$*40,cn$*11,cnt$*5,k$*25,cap$*128,p$(20)*50,io1$(2)
00080   dim cnam$*40,message$*40
00090 ! ______________________________________________________________________
00100   let fntop(program$,cap$="Name and Number List")
00110   let fncno(cno,cnam$) !:
        let fndat(dat$)
00120 ! 
00125   let fnconsole(1)
00130 ! ______________________________________________________________________
00140 ! ______________________________________________________________________
00150   open #1: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00160   open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00170   let prtcat$="N"
00180   if fnprocess=1 then goto L460
00190 ! ______________________________________________________________________
00200   print newpage
00210   close #101: ioerr L220
00220 L220: open #101: "SRow=9,SCol=14,ERow=14,ECol=66,Border=DR,Caption=<"&cap$,display,outin 
00230   print #101: newpage
00240   print #101,fields "1,1,Cc 53,R,N": cnam$
00250   print #101,fields "2,1,Cc 53,R,N": "Company Number "&str$(cno)
00260   print #101,fields "4,2,Cr 30,N": "Report Heading Date:"
00270   print #101,fields "5,2,Cr 30,n": "Print Category Names (Y/N):"
00280   let io1$(1)="4,33,C 20,UT,N"
00290   let io1$(2)="5,33,Cu 1,UT,N"
00300   print fields "15,30,C 10,B,1": "Print (F1)"
00310   print fields "15,41,C 09,B,5": "Exit (F5)"
00320 L320: rinput #101,fields mat io1$: dat$,prtcat$ conv L320
00330   if ce>0 then let io1$(ce)(ce1:ce2)="U": let ce=0
00340   if cmdkey>0 then goto L410 else let ce=curfld
00350 L350: let ce=ce+1: if ce>udim(io1$) then let ce=1
00360 L360: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : let ce1=pos(io1$(ce),"U",9) !:
        if ce1=0 then goto L350
00370   let ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L320
00380 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00390   let ce=cnt+1
00400 ERR1: print fields "24,78,C 1": bell : goto L360
00410 L410: if cmdkey=5 then goto XIT
00420   if prtcat$<>"Y" and prtcat$<>"N" then let ce=2: goto ERR1
00430   let dattab=60-len(rtrm$(dat$))/2
00440   let fndat(dat$,2)
00450 ! ______________________________________________________________________
00460 L460: print newpage
00470   let message$="Printing: please wait"
00480   let fnwait (102,cap$,message$,1)
00490   on fkey 5 goto L710
00500   let fnopenprn
00510   gosub HDR
00520 L520: read #1,using L530: jn$,n$ eof L680
00530 L530: form pos 1,c 6,c 40
00540   let first=0
00550   form skip 2,pos 1,c 6,pos 10,c 40,skip 0
00560   goto L590
00570   print #255: newpage
00580   gosub HDR
00590 L590: if prtcat$="N" then goto L520
00600   let cnt$="    0"
00610   read #2,using L640,key>=jn$&cnt$: cn$,k$ exit L680
00620   goto L650
00630 L630: read #2,using L640: cn$,k$ exit L680
00640 L640: form pos 1,c 11,c 25
00650 L650: if cn$(1:6)><jn$ then goto L520
00660   gosub L840
00670   goto L630
00680 L680: exit eof L690,nokey L690
00690 L690: close #1: 
00700   close #2: 
00710 L710: let fncloseprn
00720   goto XIT
00730 ! ______________________________________________________________________
00740 HDR: ! 
00750   print #255,using L760: "Job Name and Number Listing",dat$
00760 L760: form skip 3,pos 47,c 34,skip 1,pos dattab,c 20,skip 1
00770   if prtcat$="N" then print #255,using L780: "  Job #   Job Name"
00780 L780: form pos 1,c 50,skip 0
00790   if prtcat$<>"N" then print #255,using L800: "  Job #   Job Name","Category #   Category Name"
00800 L800: form pos 1,c 50,pos 47,c 50,skip skh
00810   let skh=1
00820   return 
00830 ! ______________________________________________________________________
00840 L840: if first=0 then print #255,using L842: jn$,n$,cn$(7:11),k$ pageoflow L870: let first =1: goto L860
00842 L842: form skip 2,pos 1,c 6,pos 10,c 40,x 1,c 5,pos 60,c 25,skip 1
00845   print #255,using L850: cn$(7:11),k$ pageoflow L870
00850 L850: form pos 51,c 5,pos 60,c 25
00860 L860: goto L890
00870 L870: print #255: newpage
00880   gosub HDR
00890 L890: return 
00900 ! ______________________________________________________________________
00910 XIT: let fnxit
00920 ! ______________________________________________________________________
00930 ! <Updateable Region: ERTN>
00940 ERTN: let fnerror(program$,err,line,act$,"xit")
00950   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00960   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00970   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00980 ERTN_EXEC_ACT: execute act$ : goto ERTN
00990 ! /region
01000 ! ______________________________________________________________________
