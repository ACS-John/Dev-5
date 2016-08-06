00010 ! Replace R:\acsCL\GLPost2
00020 ! POST FUND TRANSFERS TO GL
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fnerror,fncno,fnchain
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,de$*35,ta(2),td$*30,prd(23)
00080   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00090   let fncno(cno,cnam$)
00100 ! 
00110   print newpage
00120   close #101: ioerr L130
00130 L130: open #101: "SROW=08,SCOL=18,EROW=15,ECOL=62,BORDER=DR,CAPTION=POST FUND TRANSFERS TO GL",display,outin 
00140   print fields "08,18,Cc 45,R,N": cnam$
00150   print fields "09,18,Cc 45,R,N": "Company Number "&str$(cno)
00160   print fields "11,57,C 6,N": "(0=NO)"
00170   print fields "11,20,C 35": "ENTER 1 TO POST FUND TRANSFERS:"
00180   print fields "13,20,C 40": "ENTER 1 TO POST DIRECTLY TO GL OR 2"
00190   print fields "14,20,C 40": "TO TRANSFER TO DISKETTE FOR ACCOUNTANT:"
00200   let io1$(1)="11,52,N 1,UT,N" : let io1$(2)="14,60,N 1,UT,N"
00210   print fields "16,23,C 34,R,N": "PRESS F1 TO CONTINUE OR F5 TO STOP"
00220 L220: input fields mat io1$: in1,in3 conv L220
00230   let in2=0 ! DONT POST AP ANYMORE
00240   if cmdkey=5 then goto XIT
00250   if ce>0 then let io1$(ce)(ce1:ce2)="U": let ce=0
00260   if cmdkey>0 then goto L330 else let ce=curfld
00270 L270: let ce=ce+1: if ce>udim(io1$) then let ce=1
00280 L280: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : let ce1=pos(io1$(ce),"U",1) !:
        if ce1=0 then goto L270
00290   let ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L220
00300 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00310   let ce=cnt+1
00320 ERR1: print fields "24,78,C 1": bell : goto L280
00330 L330: if in1<0 or in1>1 then let ce=1: goto ERR1
00340   if in3<1 or in3>2 then let ce=2: goto ERR1
00350   print newpage
00360   close #101: ioerr L370
00370 L370: open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,BORDER=DR,CAPTION=POST GL ENTRIES",display,outin 
00380   print fields "08,18,Cc 41,R,N": cnam$
00390   print fields "09,18,Cc 41,R,N": "Company Number "&str$(cno)
00400   print fields "11,18,Cc 41,N": "processing..."
00410   print fields "13,30,C 16,B,5": "Cancel (F5)"
00420   on fkey 5 goto XIT
00430   let d2$=cnvrt$("PIC(######)",d2)
00440   open #1: "Name=Q:\GLmstr\GLBUCKET.H"&str$(cno)&",Shr",internal,input,relative ioerr L480
00450   read #1,using L460,rec=1: glb norec L470
00460 L460: form pos 1,n 1
00470 L470: close #1: 
00480 L480: if glb><2 then let glwk$="Q:\GLmstr\GLWk1"&wsid$&".h"&str$(cno)
00490   open #3: "Name=Q:\CLmstr\GLWK101.h"&str$(cno)&",SIZE=0,RecL=104,Replace",internal,output 
00500   if glb<>2 then goto L620
00510   print newpage
00520   close #101: ioerr L530
00530 L530: open #101: "SROW=08,SCOL=15,EROW=12,ECOL=62,BORDER=DR,CAPTION=POSTING DATE",display,outin 
00540   print fields "08,18,Cc 41,R,N": cnam$
00550   print fields "09,18,Cc 41,R,N": "Company Number "&str$(cno)
00560   print fields "11,18,C 41,N": "ENTER THE POSTING DATE (MMDDYY):"
00570   print fields "13,25,C 32,R,N": "Press F1 to continue; F5 to stop"
00580 L580: input fields "11,50,N 6,UE,N": postdat conv L580
00590   if postdat<10100 or postdat>123199 then goto L580
00600   print newpage
00610   if glb=2 then let glwk$="Q:\GLmstr\GL"&cnvrt$("pic(######)",postdat)&".H"&str$(cno)
00620 L620: if in1=0 then goto L740
00630   open #1: "Name=Q:\CLmstr\GLFUNDTR.h"&str$(cno)&",KFName=Q:\CLmstr\GLFUNDX1.h"&str$(cno)&"",internal,outin,keyed 
00640 L640: read #1,using L650: gl$,tr4,tr5 eof L740
00650 L650: form pos 1,c 12,n 6,pd 6.2
00660   if tr5=0 then goto L640
00670   if ap1=1 and in2=2 then let tr5=-tr5
00680   if ap1=0 then let td$="INTER-FUND TRANSFERS" else let td$="ACCOUNTS PAYABLE"
00690   let tr$=" JE-"&cnvrt$("PIC(ZZ/ZZ/ZZ)",tr4)
00700   write #3,using L710: gl$,tr4,tr5,3,0,tr$,td$,"","","","",""
00710 L710: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,c 8,c 6,c 5,c 3,c 12
00720   goto L640
00730 ! ______________________________________________________________________
00740 L740: close #1: ioerr L750
00750 L750: if ap1=1 then goto L800
00760   let ap1=1
00770   if in2=0 then goto L800
00780   open #1: "Name=Q:\CLmstr\GLAPTR.h"&str$(cno)&",KFName=Q:\CLmstr\GLAPIDX1.h"&str$(cno)&"",internal,outin,keyed 
00790   goto L640
00800 L800: close #3: 
00810   if in3=2 then goto L860
00820   if glb=2 then goto BUCKET
00830   execute "COPY Q:\CLmstr\GLWK101.h"&str$(cno)&" Q:\GLmstr\GLWK1"&wsid$&"*.*"
00840   let fnchain("R:\acsGL\ACGLMRGE")
00850 ! ______________________________________________________________________
00860 L860: print newpage
00870   print fields "10,5,C 60": "INSERT BLANK FORMATED DISKETTE INTO SELECTED DRIVE: A"
00880   print fields "12,20,C 22,R,N": "PRESS ENTER WHEN READY"
00890 L890: input fields "10,57,CU 1,UE,N": dv$
00900   if dv$="A" or dv$="B" then goto L910 else goto L890
00910 L910: let dv$=" "&dv$&":"
00920   execute "COPY Q:\CLmstr\GLWK101.h"&str$(cno)&""&dv$
00930   open #20: "Name=A:GLWK201.H"&str$(cno)&",SIZE=0,RecL=110",internal,output 
00940   close #20: 
00950   goto XIT
00960 ! ______________________________________________________________________
00970 BUCKET: dim r9$*104 ! MOVE TO GLBUCKET
00980   open #3: "Name=Q:\CLmstr\GLWK101.h"&str$(cno)&"",internal,input 
00990   open #9: "Name="&glwk$&",RecL=104,USE",internal,output 
01000 L1000: read #3,using L1010: r9$ eof XIT
01010 L1010: form pos 1,c 104
01020   write #9,using L1010: r9$
01030   goto L1000
01040 XIT: let fnxit
01050 ! ______________________________________________________________________
01060 ! <Updateable Region: ERTN>
01070 ERTN: let fnerror(cap$,err,line,act$,"xit")
01080   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01090   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01100   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01110 ERTN_EXEC_ACT: execute act$ : goto ERTN
01120 ! /region
