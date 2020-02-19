00010 ! Replace S:\acsPR\jcPrtDET
00020 ! pr Job Cost Report
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fncno,fnerror,fnopenprn,fncloseprn,fnprocess,fnconsole
00050   on error goto Ertn
00060 !
00070   dim jn$*6,holdjn$*6,n$*40,a$(3)*30,b(4),totall(7)
00080   dim sc1$(6),sd1$(6),se1$(6)*50,prtj$(100)*6,cnam$*40,npj$(2)*6
00090   dim jtot$(100)*30,jobtot(100),tottot$(100)*30,tottot(100),totjob(7)
00100   dim dcode$(100)*3,desc$(100)*30,cdesc$(100)*30,cattot(100)
00110   dim cn$*11,holdcn$*11,cnt$*5,k$*25,l(13),ta(2),eno$*12,jno$*6,tr(9)
00120   dim io1$(6)*21,pd$*30,message$*40,cap$*128
00130 !
00140   fntop(program$,cap$="Job Cost Report")
00150   fncno(cno)
00155   fnconsole(1)
00160   dat1=date("mmddyy")
00170 ! 
00180   prtjob$="N" : prtdet$="N" : sumcat$="N" : sumjob$="N" !:
        prtpag$="N" ! setup defaults to answers (also used by fnprocess=1)
00190 !
00200   open #1: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",internal,input,keyed 
00210   for j=1 to 100
00220     read #1,using 'Form POS 1,C 3,C 30': dcode$(j),desc$(100) eof L250
00230     desc$(val(dcode$(j)))=desc$(100) conv L240
00240 L240: next j
00250 L250: close #1: 
00260   desc$(100)="Unassigned"
00270 !
00280   open #20: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input,relative  !:
        read #20,using 'Form POS 1,C 40,POS 746,2*C 6',rec=1: cnam$,mat npj$ !:
        close #20: 
00290   open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed 
00300   open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed 
00310   open #3: "Name=[Q]\PRmstr\JCTRANS.h[cno],Shr",internal,input,relative 
00320 !
00330   pr newpage
00340   if fnprocess=1 then goto L640
00350   fnopenwin(win=102,7,15,16,65,cap$)
00360   pr #win,fields "4,2,Cr 42,N": "Date for Job Cost Detail Listing (mmddyy):"
00370   pr #win,fields "5,2,Cr 42,N": "Print all Jobs (Y/N):"
00380   pr #win,fields "6,2,Cr 42,N": "Print Details (Y/N):"
00390   pr #win,fields "7,2,Cr 42,N": "Summarize by Category (Y/N):"
00400   pr #win,fields "8,2,Cr 42,N": "Summarize by Job (Y/N):"
00410   pr #win,fields "9,2,Cr 42,N": "Start Jobs on a new page (Y/N):"
00420   io1$(1)="4,45,pic(zzzzzz),UT,N"
00430   for j=2 to 6
00440     io1$(j)=str$(j+3)&",45,Cu 1,UT,N"
00450   next j
00460   pr f "17,30,C 09,B,1": "Next (F1)"
00470   pr f "17,41,C 09,B,5": "Exit (F5)"
00480 L480: rinput #win,fields mat io1$: dat1,prtjob$,prtdet$,sumcat$,sumjob$,prtpag$ conv CONV1
00490   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
00500   if cmdkey>0 then goto L570 else ce=curfld
00510 L510: ce=ce+1: if ce>udim(io1$) then ce=1
00520 L520: io1$(ce)=rtrm$(io1$(ce)) : ce1=pos(io1$(ce),"U",1) !:
        if ce1=0 then goto L510
00530   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L480
00540 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
00550   ce=cnt+1
00560 ERR1: pr f "24,78,C 1": bell : goto L520
00570 L570: if cmdkey=5 then goto XIT
00580   if dat1<10100 or dat1>123199 then ce=1: goto ERR1
00590   if prtjob$<>"Y" and prtjob$<>"N" then ce=2: goto ERR1
00600   if prtdet$<>"Y" and prtdet$<>"N" then ce=3: goto ERR1
00610   if sumcat$<>"Y" and sumcat$<>"N" then ce=4: goto ERR1
00620   if sumjob$<>"Y" and sumjob$<>"N" then ce=5: goto ERR1
00630   if prtpag$<>"Y" and prtpag$<>"N" then ce=6: goto ERR1
00640 L640: if prtdet$="N" and sumcat$="N" and sumjob$="N" then goto L650 else noread=1
00650 L650: if prtjob$="Y" then goto L810
00660   for k=1 to 100
00670     pr newpage
00680     fnopenwin(win=102,10,20,15,59,cap$)
00690     if k=1 then goto L710
00700     if k>1 then pr #win,fields "6,1,Cc 40,R,N": "Last Job Number entered was "&prtj$(k-1)
00710 L710: pr #win,fields "4,2,C 20,N": "Job Number to print:"
00720     pr f "16,34,C 11,B,2": "Print (F2)"
00730 L730: input #win,fields "4,23,C 6,UT,N": prtj$(k) conv L730
00740     if cmdkey=2 then goto L800
00750     if rtrm$(prtj$(k))="" or ltrm$(rtrm$(prtj$(k)))="0" then goto L730
00760     prtj$(k)=lpad$(rtrm$(prtj$(k)),6)
00770   next k
00780   goto L810
00790 !
00800 L800: k=k-1
00810 L810: pr newpage
00820   fnwait(message$="Printing: please wait...",1)
00830   on fkey 5 goto DONE
00840   fnopenprn
00850 L850: if prtjob$="Y" then goto L940
00860 L860: j1=j1+1
00870   if j1<=k then goto L910
00880   eofc=2
00890   goto L1890
00900 !
00910 L910: read #1,using L920,key=prtj$(j1): jn$,n$,mat a$,mat b nokey L860
00920 L920: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
00930   goto L960
00940 L940: read #1,using L920: jn$,n$,mat a$,mat b eof L1860
00950   if jn$>=npj$(1) and jn$<=npj$(2) then goto L940
00960 L960: if hd=1 then goto L1010
00970   gosub HDR
00980   hd=1
00990   goto L1090
01000 !
01010 L1010: if prtpag$="N" then goto L1080
01020   pr #255: newpage
01030   hd=0
01040   gosub HDR
01050   hd=1
01060   goto L1090
01070 !
01080 L1080: gosub HDR
01090 L1090: cnt$="    0"
01100 L1100: read #2,using L1120,key>=jn$&cnt$: cn$,k$,mat l,mat ta nokey L1860
01110   detcd=0
01120 L1120: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
01130   tr56=0
01140   tr8=0
01150   tr9=0
01160   tr89=0
01170   fstdet=0
01180   if prtjob$="Y" then goto L1220
01190   if cn$(1:6)><prtj$(j1) then goto L1670
01200   goto L1230
01210 !
01220 L1220: if cn$(1:6)><jn$ then goto L1670
01230 L1230: if noread=0 then goto L1330
01240   if ta(1)=0 and ta(2)=0 then goto L1510
01250   nta=ta(1)
01260 L1260: read #3,using L1280,rec=nta: eno$,jno$,mat tr,pd$,nta
01270   if tr(2)=0 then tr(2)=100
01280 L1280: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
01290   if prtdet$="N" then goto L1330
01300   gosub PRINTDETAILLINE
01310   goto L1360
01320 !
01330 L1330: if detcd=1 then goto L1360
01340   detcd=1
01350   gosub TOTALLINEWITHOUTDETAILS
01360 L1360: if sumcat$="N" and subjob$="N" then goto L1390
01370   if noread=0 then goto L1530
01380   gosub ACCUMULATEFORCATEGORYSUMMARY
01390 L1390: if nta=0 then goto L1420
01400   goto L1260
01410 !
01420 L1420: if prtdet$="N" then goto L1530
01430   if tr8+tr9+tr89=l(4)+l(6) and tr56=l(5) then goto L1460
01440   pr #255,using L1450: "Previous Balance",l(5)-tr56,l(4)-tr9,l(6)-(tr8+tr89)
01450 L1450: form skip 1,pos 20,c 16,pos 36,n 9.2,2*n 14.2,skip 1
01460 L1460: pr #255,using L1470: "________","____________","____________"
01470 L1470: form pos 37,c 10,2*c 14,skip 1
01480   pr #255,using L1490: "Total "&k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
01490 L1490: form pos 10,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
01500   goto L1530
01510 L1510: pr #255,using L1520: cn$(7:11)&"   "&k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
01520 L1520: form pos 1,c 28,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
01530 L1530: totjob(1)=totjob(1)+l(5)
01540   totjob(2)=totjob(2)+l(4)
01550   totjob(3)=totjob(3)+l(6)
01560   totjob(4)=totjob(4)+l(1)
01570   totjob(5)=totjob(5)+l(3)
01580   totjob(6)=totjob(6)+(l(1)-l(4))
01590   totjob(7)=totjob(7)+(l(3)-l(6))
01600   if noread=0 then goto L1640
01610   form pos 9,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14,skip 2
01620   if ta(1)=0 and ta(2)=0 then goto L1640
01630   gosub PRINTCATEGORYSUMMARY
01640 L1640: cnt$=lpad$(rtrm$(str$(val(cn$(7:11))+1)),5)
01650   goto L1100
01660 !
01670 L1670: if eofc=2 then goto L1690
01680   goto L1720
01690 L1690: pr #255: newpage
01700   hd=0
01710   gosub HDR
01720 L1720: pr #255,using L1790: "---------","-----------","-----------","------------","------------","------------","------------"
01730   if eofc=2 then goto L1760
01740   pr #255,using L1770: "Total by Job",mat totjob
01750   goto L1770
01760 L1760: pr #255,using L1770: "Totals For All Jobs",mat totjob
01770 L1770: form pos 9,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2
01780   pr #255,using L1790: "=========","===========","===========","============","============","============","============" pageoflow NWPGE
01790 L1790: form pos 35,c 13,2*c 14,pos 84,4*c 14,skip 2
01800   if eofc=2 then goto L1950
01810   mat totall=totall+totjob
01820   mat totjob=(0)
01830   if sumjob$="N" then goto L1850
01840   gosub PRINTSUMMARYBYJOB
01850 L1850: if eofc=1 then goto L1890 else goto L850
01860 L1860: eofc=1
01870   goto L1670
01880 !
01890 L1890: if sumjob$="N" or prtdet$="N" then goto L1920 ! END OF JOB ROUTINE
01900   if eofc=1 or eofc=2 then goto L1920
01910   gosub PRINTSUMMARYBYJOB
01920 L1920: eofc=2
01930   mat totjob=totall
01940   goto L1670
01950 L1950: if sumjob$="N" then goto L1970
01960   gosub PRINTSUMMARYOFALLJOBS
01970 L1970: close #1: 
01980   close #2: 
01990   close #3: 
02000 DONE: ! 
02010   fncloseprn
02020   goto XIT
02030 !
02040 HDR: ! 
02050   if hd=1 then goto L2080
02060   pr #255,using L2070: cnam$,"Job Cost Detail Listing","As of",dat1
02070 L2070: form pos 1,cc 122,skip 1,pos 55,c 23,skip 1,pos 60,c 6,pic(zz/zz/zz),skip 1
02080 L2080: if eofc=0 then pr #255,using L2090: "Job Number "&jn$,n$ !:
          ! pr SUB-HEADING
02090 L2090: form skip 1,pos 30,c 17,pos 65,c 40,skip 2
02100   pr #255,using L2110: "Category","Category","Reference","Labor","Other","Estimated Cost","Over/Under"
02110 L2110: form pos 1,c 8,pos 11,c 8,pos 21,c 9,pos 54,c 5,pos 68,c 5,pos 92,c 14,pos 124,c 10,skip 1
02120   pr #255,using L2130: "Number","Description","Number","Date","Hours","Cost","Cost","Sub-Category","Labor","Other","Labor","Other" pageoflow NWPGE
02130 L2130: form pos 1,c 6,pos 9,c 11,pos 22,c 6,pos 30,c 4,pos 40,c 5,pos 55,c 4,pos 69,c 4,pos 76,c 12,pos 91,c 5,pos 105,c 5,pos 119,c 5,pos 133,c 5,skip 2
02140   return 
02150 !
02160 PRINTDETAILLINE: ! 
02170   if fstdet=1 then goto L2210
02180   fstdet=1
02190   pr #255,using L2200: tr(1),k$(1:20)
02200 L2200: form pos 1,n 5,pos 9,c 20,skip 1
02210 L2210: if tr(5)+tr(6)><0 then goto L2290
02220   if rtrm$(pd$)="" then goto L2250
02230   pr #255,using L2260: eno$,tr(4),0,0,tr(8)+tr(9),tr(2)," - "&pd$(1:25) pageoflow NWPGE
02240   goto L2260
02250 L2250: pr #255,using L2260: eno$,tr(4),0,0,tr(8)+tr(9),tr(2)," - "&desc$(tr(2))(1:25) pageoflow NWPGE
02260 L2260: form skip 1,pos 15,c 12,pos 28,pic(zz/zz/zz),pos 36,n 9.2,2*n 14.2,pos 74,n 4,c 28,skip 1
02270   tr89=tr89+tr(8)+tr(9)
02280   goto L2360
02290 L2290: if rtrm$(pd$)="" then goto L2320
02300   pr #255,using L2260: eno$,tr(4),tr(5)+tr(6),tr(9),tr(8),tr(2)," - "&pd$(1:25) pageoflow NWPGE
02310   goto L2330
02320 L2320: pr #255,using L2260: eno$,tr(4),tr(5)+tr(6),tr(9),tr(8),tr(2)," - "&desc$(tr(2))(1:25) pageoflow NWPGE
02330 L2330: tr56=tr56+tr(5)+tr(6)
02340   tr9=tr9+tr(9)
02350   tr8=tr8+tr(8)
02360 L2360: return 
02370 !
02380 ACCUMULATEFORCATEGORYSUMMARY: ! 
02390   cattot(tr(2))=cattot(tr(2))+tr(8)+tr(9)
02400   cdesc$(tr(2))=desc$(tr(2))
02410   if lcat=0 and hcat=0 then goto L2420 else goto L2450
02420 L2420: lcat=tr(2)
02430   hcat=lcat
02440   goto L2470
02450 L2450: if tr(2)<lcat then lcat=tr(2)
02460   if tr(2)>hcat then hcat=tr(2)
02470 L2470: return 
02480 !
02490 PRINTCATEGORYSUMMARY: ! 
02500   if sumcat$="N" then goto L2590 else pr #255,using L2510: "************** Summary by Category *************"
02510 L2510: form pos 90,cc 50,skip 1
02520 L2520: form pos 90,c 50,skip 2
02530   for j=lcat to hcat
02540     if cattot(j)=0 then goto L2570
02550     pr #255,using L2560: cdesc$(j),cattot(j) pageoflow NWPGE
02560 L2560: form pos 90,c 30,pos 124,n 14.2,skip 1
02570 L2570: next j
02580   pr #255,using L2520: "************************************************"
02590 L2590: mat jobtot=jobtot+cattot
02600   for j=lcat to hcat
02610     if rtrm$(cdesc$(j))="" then goto L2630
02620     jtot$(j)=cdesc$(j)
02630 L2630: next j
02640   mat cattot=(0)
02650   lcat=0
02660   hcat=0
02670   return 
02680 !
02690 PRINTSUMMARYBYJOB: ! 
02700   if sumjob$="N" then goto L2790
02710   pr #255,using L2510: "***************** SUMMARY BY JOB ***************"
02720   form skip 1,pos 90,c 27,skip 2
02730   for j=1 to 100
02740     if jobtot(j)=0 then goto L2770
02750     pr #255,using L2760: jtot$(j),jobtot(j) pageoflow NWPGE
02760 L2760: form pos 90,c 30,pos 124,n 14.2,skip 1
02770 L2770: next j
02780   pr #255,using L2520: "************************************************"
02790 L2790: mat tottot=tottot+jobtot
02800   for j=1 to 100
02810     if rtrm$(jtot$(j))="" then goto L2830
02820     tottot$(j)=jtot$(j)
02830 L2830: next j
02840   mat jobtot=(0)
02850   return 
02860 !
02870 TOTALLINEWITHOUTDETAILS: ! 
02880   pr #255,using L2890: val(cn$(7:11)),k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
02890 L2890: form pos 1,n 5,pos 9,c 20,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
02900   return 
02910 !
02920 PRINTSUMMARYOFALLJOBS: ! 
02930   if sumcat$="N" and sumjob$="N" then goto L3020
02940   pr #255,using L2510: "*************** Summary of All Jobs ************"
02950   form skip 1,pos 90,c 27,skip 2
02960   for j=1 to 100
02970     if tottot(j)=0 then goto L3000
02980     pr #255,using L2990: tottot$(j),tottot(j) pageoflow NWPGE
02990 L2990: form pos 90,c 30,pos 124,n 14.2,skip 1
03000 L3000: next j
03010   pr #255,using L2520: "************************************************"
03020 L3020: return 
03030 !
03040 NWPGE: ! 
03050   pr #255: newpage
03060   hd=0
03070   gosub HDR
03080   hd=1
03090   continue 
03100 !
03110 XIT: fnxit
03120 !
03130 ! <Updateable Region: ERTN>
03140 ERTN: fnerror(program$,err,line,act$,"xit")
03150   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03160   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03170   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
03180 ERTN_EXEC_ACT: execute act$ : goto ERTN
03190 ! /region
03200 !
