00010 ! Replace S:\acsPR\jcStWkSh
00020 ! pr Job Status Worksheet
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fnoldmsgbox, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnprocess,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim jn$*6,n$*40,next$*5,cn$*11,cnt$*5,k$*25,cap$*128,response$(5)*1
00080   dim sc1$(3),sd1$(3),se1$(3)*50,prtj$(100)*6,dat$*20,message$*40
00090   dim msgline$(2)*60
00100 ! ______________________________________________________________________
00110   fntop("S:\acsPR\jcStWkSh",cap$="Job Status Worksheet")
00120   fncno(cno) !:
        fndat(dat$)
00130 ! 
00135   fnconsole(1)
00140   let prtjob$="N" : let perpag$="N"
00150   open #1: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00160   open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00170 ! ______________________________________________________________________
00180   if fnprocess=1 then goto L590 ! goto "asdf"
00190   pr newpage
00200   fnopenwin(win=101,09,14,15,65,cap$)
00210   pr #win,fields "4,2,Cr 29,N": "Report Heading Date:"
00220   pr #win,fields "5,2,Cr 29,N": "Print All Jobs (Y/N):"
00230   pr #win,fields "6,2,Cr 29,N": "Print One Job Per Page (Y/N):"
00240   let io1$(1)="4,32,C 20,UT,N"
00250   let io1$(2)="5,32,Cu 1,UT,N"
00260   let io1$(3)="6,32,Cu 1,UT,N"
00270   pr f "16,30,C 09,B,1": "Next (F1)"
00280   pr f "16,41,C 09,B,5": "Exit (F5)"
00290   let prtjob$="Y"
00300   let perpag$="Y"
00310 L310: rinput #win,fields mat io1$: dat$,prtjob$,perpag$ conv CONV1
00320   if ce>0 then let io1$(ce)(ce1:ce2)="U": ce=0
00330   if cmdkey>0 then goto L400 else ce=curfld
00340 L340: ce=ce+1: if ce>udim(io1$) then ce=1
00350 L350: let io1$(ce)=rtrm$(io1$(ce)) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L340
00360   ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L310
00370 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00380   ce=cnt+1
00390 ERR1: pr f "24,78,C 1": bell : goto L350
00400 L400: if cmdkey=5 then goto XIT
00410   if prtjob$<>"Y" and prtjob$<>"N" then goto L420 else goto L440
00420 L420: ce=2
00430   goto CONV1
00440 L440: if perpag$<>"Y" and perpag$<>"N" then goto L450 else goto L480
00450 L450: ce=3
00460   goto CONV1
00470 ! ______________________________________________________________________
00480 L480: let fndat(dat$,2)
00490   form pos 63,c 20
00500   if prtjob$="N" then goto L590 ! "asdf"
00510   if fnprocess=1 then goto L580
00520 ! ______________________________________________________________________
00530   pr newpage
00540   let msgline$(1)="Do you wish to skip all"
00550   let msgline$(2)="completed Jobs? (Y/N)"
00560   fnoldmsgbox(mat response$,cap$,mat msgline$,2)
00570   skpcom$=response$(1)
00580 L580: goto L730
00590 L590: pr newpage
00600   fnopenwin(win=103,08,20,13,59,cap$)
00610   for j=1 to 100
00620     pr #win,fields "4,2,Cr 20,n": "Job Number to print:"
00630     if j>1 then pr #win,fields "6,1,Cc 40,R,N": "Last Job Number entered was "&ltrm$(prtj$(j-1))
00640     pr f "14,34,C 11,B,2": "Print (F2)"
00650 L650: input #win,fields "4,23,C 6,UT,N": prtj$(j) conv L650
00660     if cmdkey=2 then goto L710
00670     if rtrm$(prtj$(j))="" or ltrm$(rtrm$(prtj$(j)))="0" then goto L650
00680     let prtj$(j)=lpad$(rtrm$(prtj$(j)),6)
00690   next j
00700   goto L730
00710 L710: let j=j-1
00720 ! ______________________________________________________________________
00730 L730: pr newpage
00740   on fkey 5 goto DONE
00750   fnwait(104,cap$,message$,1)
00760   fnopenprn !:
        if file$(255)(1:3)<>"PRN" then let jbskip=1
00770   gosub HDR
00780 L780: if prtjob$="Y" then goto L830
00790 L790: if j1+=1>j then goto DONE
00800   read #1,using L810,key=prtj$(j1): jn$,n$,b4 nokey L790
00810 L810: form pos 1,c 6,c 40,pos 157,n 2
00820   goto L840
00830 L830: read #1,using L810: jn$,n$,b4 eof DONE
00840 L840: if skpcom$="Y" and b4=9 then goto L780
00850   gosub L1080
00860   cnt$="    0"
00870 L870: read #2,using L880,key>=jn$&cnt$: cn$,k$,l12,l13 eof L780,nokey L780
00880 L880: form pos 1,c 11,c 25,pos 114,2*pd 2
00890   if cn$(1:6)><jn$ then goto L780
00900   gosub L1170
00910   cnt$=lpad$(rtrm$(str$(val(cn$(7:11))+1)),5)
00920   goto L870
00930 ! ______________________________________________________________________
00940 DONE: close #1: 
00950   close #2: 
00960   fncloseprn
00970   goto XIT
00980 ! ______________________________________________________________________
00990 HDR: ! 
01000   pr #255,using L1010: "Job Status Worksheet",dat$
01010 L1010: form pos 56,c 20,skip 1,pos 1,cc 132,skip 2
01020   pr #255,using L1030: "Job       Job Name  ","Category   Description","Old %    Percent     Units"
01030 L1030: form pos 2,c 20,pos 47,c 22,pos 88,c 26,skip 1
01040   pr #255,using L1050: "Number","Number","Complete Complete   Complete"
01050 L1050: form pos 1,c 6,pos 48,c 6,pos 87,c 28,skip 2
01060   return 
01070 ! ______________________________________________________________________
01080 L1080: if fst=1 then goto L1090 else goto L1120
01090 L1090: if perpag$="N" then goto L1130
01100   pr #255: newpage
01110   gosub HDR
01120 L1120: let fst=1
01130 L1130: pr #255,using L1140: jn$,n$
01140 L1140: form pos 1,c 6,pos 8,c 40,skip jbskip
01150   return 
01160 ! ______________________________________________________________________
01170 L1170: pr #255,using L1210: cn$(7:11),k$,"LABOR",l12,"%","___%","_____.__" pageoflow L1190
01180   goto L1220
01190 L1190: pr #255: newpage
01200   gosub HDR
01210 L1210: form pos 49,c 5,pos 55,c 25,pos 81,c 5,pos 89,n 3,c 1,pos 97,c 4,pos 107,c 8,skip 1
01220 L1220: pr #255,using L1210: " "," ","OTHER",l13,"%","___%","        " pageoflow L1250
01230   pr #255: pageoflow L1250
01240   goto L1270
01250 L1250: pr #255: newpage
01260   gosub HDR
01270 L1270: return 
01280 ! ______________________________________________________________________
01290 XIT: let fnxit
01300 ! ______________________________________________________________________
01310 ! <Updateable Region: ERTN>
01320 ERTN: let fnerror(program$,err,line,act$,"xit")
01330   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01340   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01350   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01360 ERTN_EXEC_ACT: execute act$ : goto ERTN
01370 ! /region
01380 ! ______________________________________________________________________
