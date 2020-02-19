00010 ! Replace S:\acsPR\jcNamLst
00020 ! pr Name and Number List
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fncloseprn,fnopenprn,fncno,fnerror,fndat,fnprocess,fnconsole
00050   on error goto Ertn
00060 !
00070   dim dat$*20,jn$*6,n$*40,cn$*11,cnt$*5,k$*25,cap$*128,p$(20)*50,io1$(2)
00080   dim cnam$*40,message$*40
00090 !
00100   fntop(program$,cap$="Name and Number List")
00110   fncno(cno,cnam$) !:
        fndat(dat$)
00120 ! 
00125   fnconsole(1)
00130 !
00140 !
00150   open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed 
00160   open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed 
00170   prtcat$="N"
00180   if fnprocess=1 then goto L460
00190 !
00200   pr newpage
00210   close #101: ioerr L220
00220 L220: open #101: "SRow=9,SCol=14,ERow=14,ECol=66,Border=DR,Caption=<"&cap$,display,outIn 
00230   pr #101: newpage
00240   pr #101,fields "1,1,Cc 53,R,N": cnam$
00250   pr #101,fields "2,1,Cc 53,R,N": "Company Number [cno]"
00260   pr #101,fields "4,2,Cr 30,N": "Report Heading Date:"
00270   pr #101,fields "5,2,Cr 30,n": "Print Category Names (Y/N):"
00280   io1$(1)="4,33,C 20,UT,N"
00290   io1$(2)="5,33,Cu 1,UT,N"
00300   pr f "15,30,C 10,B,1": "Print (F1)"
00310   pr f "15,41,C 09,B,5": "Exit (F5)"
00320 L320: rinput #101,fields mat io1$: dat$,prtcat$ conv L320
00330   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
00340   if cmdkey>0 then goto L410 else ce=curfld
00350 L350: ce=ce+1: if ce>udim(io1$) then ce=1
00360 L360: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",9) !:
        if ce1=0 then goto L350
00370   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L320
00380 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
00390   ce=cnt+1
00400 ERR1: pr f "24,78,C 1": bell : goto L360
00410 L410: if cmdkey=5 then goto XIT
00420   if prtcat$<>"Y" and prtcat$<>"N" then ce=2: goto ERR1
00430   dattab=60-len(rtrm$(dat$))/2
00440   fndat(dat$,2)
00450 !
00460 L460: pr newpage
00470   message$="Printing: please wait"
00480   fnwait(message$,1)
00490   on fkey 5 goto L710
00500   fnopenprn
00510   gosub HDR
00520 L520: read #1,using L530: jn$,n$ eof L680
00530 L530: form pos 1,c 6,c 40
00540   first=0
00550   form skip 2,pos 1,c 6,pos 10,c 40,skip 0
00560   goto L590
00570   pr #255: newpage
00580   gosub HDR
00590 L590: if prtcat$="N" then goto L520
00600   cnt$="    0"
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
00710 L710: fncloseprn
00720   goto XIT
00730 !
00740 HDR: ! 
00750   pr #255,using L760: "Job Name and Number Listing",dat$
00760 L760: form skip 3,pos 47,c 34,skip 1,pos dattab,c 20,skip 1
00770   if prtcat$="N" then pr #255,using L780: "  Job #   Job Name"
00780 L780: form pos 1,c 50,skip 0
00790   if prtcat$<>"N" then pr #255,using L800: "  Job #   Job Name","Category #   Category Name"
00800 L800: form pos 1,c 50,pos 47,c 50,skip skh
00810   skh=1
00820   return 
00830 !
00840 L840: if first=0 then pr #255,using L842: jn$,n$,cn$(7:11),k$ pageoflow L870: first =1: goto L860
00842 L842: form skip 2,pos 1,c 6,pos 10,c 40,x 1,c 5,pos 60,c 25,skip 1
00845   pr #255,using L850: cn$(7:11),k$ pageoflow L870
00850 L850: form pos 51,c 5,pos 60,c 25
00860 L860: goto L890
00870 L870: pr #255: newpage
00880   gosub HDR
00890 L890: return 
00900 !
00910 XIT: fnxit
00920 !
00930 ! <Updateable Region: ERTN>
00940 ERTN: fnerror(program$,err,line,act$,"xit")
00950   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00960   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00970   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00980 ERTN_EXEC_ACT: execute act$ : goto ERTN
00990 ! /region
01000 !
