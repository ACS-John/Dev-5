00010 ! Replace S:\acsPR\newjcPrtDET
00020 ! pr Job Cost Report
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fncno,fnerror,fnopenprn,fncloseprn,fnprocess,fntos,fnlbl,fntxt,fnchk,fnfra,fnopt,fncmdset,fnacs,fncmbjob
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim jn$*6,holdjn$*6,n$*40,a$(3)*30,b(4),totall(7)
00080   dim sc1$(6),sd1$(6),se1$(6)*50,prtj$(100)*6,cnam$*40,npj$(2)*6
00090   dim jtot$(100)*30,jobtot(100),tottot$(100)*30,tottot(100),totjob(7)
00100   dim dcode$(100)*3,desc$(100)*30,cdesc$(100)*30,cattot(100)
00110   dim cn$*11,holdcn$*11,cnt$*5,k$*25,l(13),ta(2),eno$*12,jno$*6,tr(9)
00120   dim io1$(6)*21,pd$*30,message$*40,cap$*128,resp$(6)*50
00130 ! ______________________________________________________________________
00140   fntop(program$,cap$="Job Cost Report")
00150   fncno(cno)
00160   dat1=date("mmddyy")
00170 ! 
00180   prtjob$="N" : prtdet$="N" : sumcat$="N" : sumjob$="N" !:
        prtpag$="N" ! setup defaults to answers (also used by fnprocess=1)
00190 ! ______________________________________________________________________
00200   open #1: "Name="&env$('Q')&"\PRmstr\SCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\SCIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00210   for j=1 to 100
00220     read #1,using 'Form POS 1,C 3,C 30': dcode$(j),desc$(100) eof L250
00230     desc$(val(dcode$(j)))=desc$(100) conv L240
00240 L240: next j
00250 L250: close #1: 
00260   desc$(100)="Unassigned"
00270 ! ______________________________________________________________________
00280   open #20: "Name="&env$('Q')&"\PRmstr\Company.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using 'Form POS 1,C 40,POS 746,2*C 6',rec=1: cnam$,mat npj$ !:
        close #20: 
00290   open #1: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00300   open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00310   open #3: "Name="&env$('Q')&"\PRmstr\JCTRANS.h"&str$(cno)&",Shr",internal,input,relative 
00320 ! ______________________________________________________________________
00330 L330: fntos(sn$="prtdet") !:
        mylen=25 : mypos=mylen+2: resp=0: left=1
00340   fnlbl(1,37,"",1)
00350   fnlbl(1,1,"Date of LIsting:",23,left)
00360   fntxt(1,mypos,8,0,1,"1") !:
        resp$(resp+=1)=str$(dat1)
00370   fnchk(2,mypos,"Print all Jobs:",left) !:
        resp$(resp+=1)="False"
00380   fnchk(3,mypos,"Print Details:",left) !:
        resp$(resp+=1)="False"
00390   fnchk(4,mypos,"Summarize by Category:",left) !:
        resp$(resp+=1)="False"
00400   fnchk(5,mypos,"Summarize by Job:",left) !:
        resp$(resp+=1)="False"
00410   fnchk(6,mypos,"Start Jobs On a New Page:",left) !:
        resp$(resp+=1)="False"
00420   fncmdset(2)
00430   fnacs(sn$,0,mat resp$,ck)
00440   if ck=5 then goto XIT
00450   dat1=val(resp$(1)) ! date
00460   if resp$(2)="True" then prtjob$="Y" else prtjob$="N"
00470   if resp$(3)="True" then prtdet$="Y" else prtdet$="N"
00480   if resp$(4)="True" then sumcat$="Y" else sumcat$="N"
00490   if resp$(5)="True" then sumjob$="Y" else sumjob$="N"
00500   if resp$(6)="True" then prtpag$="Y" else prtpag$="N"
00510   if prtdet$="N" and sumcat$="N" and sumjob$="N" then goto L520 else noread=1
00520 L520: if prtjob$="Y" then goto L700
00530 ! ______________________________________________________________________
00540 ASK_JOB: ! 
00550   for k=1 to 100
00560     fntos(sn$="prtdet2") !:
          mylen=12 : mypos=mylen+3: resp=0: left=1
00570     fnlbl(1,1,"Job Number:",mylen,1) !:
          fncmbjob(1,mypos) !:
          resp$(respc+=1)=jn$
00580     if k=1 then goto L600
00590     if k>1 then let fnlbl(3,1,"Last Job Number entered was "&prtj$(k-1),50,1)
00600 L600: fncmdset(2)
00610     fnacs(sn$,0,mat resp$,ck)
00620     if ck=5 then goto L690
00630     prtj$(k)=lpad$(trim$(resp$(1)(1:6)),6)
00640     if rtrm$(prtj$(k))="" or ltrm$(rtrm$(prtj$(k)))="0" then goto L330
00650     prtj$(k)=lpad$(rtrm$(prtj$(k)),6)
00660   next k
00670   goto L700
00680 ! ______________________________________________________________________
00690 L690: k=k-1
00700 L700: on fkey 5 goto DONE
00710   fnopenprn
00720 L720: if prtjob$="Y" then goto L810
00730 L730: j1=j1+1
00740   if j1<=k then goto L780
00750   eofc=2
00760   goto L1760
00770 ! ______________________________________________________________________
00780 L780: read #1,using L790,key=prtj$(j1): jn$,n$,mat a$,mat b nokey L730
00790 L790: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
00800   goto L830
00810 L810: read #1,using L790: jn$,n$,mat a$,mat b eof L1730
00820   if jn$>=npj$(1) and jn$<=npj$(2) then goto L810
00830 L830: if hd=1 then goto L880
00840   gosub HDR
00850   hd=1
00860   goto L960
00870 ! ______________________________________________________________________
00880 L880: if prtpag$="N" then goto L950
00890   pr #255: newpage
00900   hd=0
00910   gosub HDR
00920   hd=1
00930   goto L960
00940 ! ______________________________________________________________________
00950 L950: gosub HDR
00960 L960: cnt$="    0"
00970 L970: read #2,using L990,key>=jn$&cnt$: cn$,k$,mat l,mat ta nokey L1730
00980   detcd=0
00990 L990: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
01000   tr56=0
01010   tr8=0
01020   tr9=0
01030   tr89=0
01040   fstdet=0
01050   if prtjob$="Y" then goto L1090
01060   if cn$(1:6)><prtj$(j1) then goto L1540
01070   goto L1100
01080 ! ______________________________________________________________________
01090 L1090: if cn$(1:6)><jn$ then goto L1540
01100 L1100: if noread=0 then goto L1200
01110   if ta(1)=0 and ta(2)=0 then nta=0: goto L1200
01120   nta=ta(1)
01130 L1130: read #3,using L1150,rec=nta: eno$,jno$,mat tr,pd$,nta
01140   if tr(2)=0 then tr(2)=100
01150 L1150: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
01160   if prtdet$="N" then goto L1200
01170   gosub PRINTDETAILLINE
01180   goto L1230
01190 ! ______________________________________________________________________
01200 L1200: if detcd=1 then goto L1230
01210   detcd=1
01220   gosub TOTALLINEWITHOUTDETAILS
01230 L1230: if sumcat$="N" and subjob$="N" then goto L1260
01240   if noread=0 then goto L1400
01250   gosub ACCUMULATEFORCATEGORYSUMMARY
01260 L1260: if nta=0 then goto L1290
01270   goto L1130
01280 ! ______________________________________________________________________
01290 L1290: if prtdet$="N" then goto L1400
01300   if tr8+tr9+tr89=l(4)+l(6) and tr56=l(5) then goto L1330
01310   pr #255,using L1320: "Previous Balance",l(5)-tr56,l(4)-tr9,l(6)-(tr8+tr89)
01320 L1320: form skip 1,pos 20,c 16,pos 36,n 9.2,2*n 14.2,skip 1
01330 L1330: pr #255,using L1340: "________","____________","____________"
01340 L1340: form pos 37,c 10,2*c 14,skip 1
01350   pr #255,using L1360: "Total "&k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
01360 L1360: form pos 10,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
01370   goto L1400
01380   pr #255,using L1390: cn$(7:11)&"   "&k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
01390 L1390: form pos 1,c 28,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
01400 L1400: totjob(1)=totjob(1)+l(5)
01410   totjob(2)=totjob(2)+l(4)
01420   totjob(3)=totjob(3)+l(6)
01430   totjob(4)=totjob(4)+l(1)
01440   totjob(5)=totjob(5)+l(3)
01450   totjob(6)=totjob(6)+(l(1)-l(4))
01460   totjob(7)=totjob(7)+(l(3)-l(6))
01470   if noread=0 then goto L1510
01480   form pos 9,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14,skip 2
01490   if ta(1)=0 and ta(2)=0 then goto L1510
01500   gosub PRINTCATEGORYSUMMARY
01510 L1510: cnt$=lpad$(rtrm$(str$(val(cn$(7:11))+1)),5)
01520   goto L970
01530 ! ______________________________________________________________________
01540 L1540: if eofc=2 then goto L1560
01550   goto L1590
01560 L1560: pr #255: newpage
01570   hd=0
01580   gosub HDR
01590 L1590: pr #255,using L1660: "---------","-----------","-----------","------------","------------","------------","------------"
01600   if eofc=2 then goto L1630
01610   pr #255,using L1640: "Total by Job",mat totjob
01620   goto L1640
01630 L1630: pr #255,using L1640: "Totals For All Jobs",mat totjob
01640 L1640: form pos 9,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2
01650   pr #255,using L1660: "=========","===========","===========","============","============","============","============" pageoflow NWPGE
01660 L1660: form pos 35,c 13,2*c 14,pos 84,4*c 14,skip 2
01670   if eofc=2 then goto L1820
01680   mat totall=totall+totjob
01690   mat totjob=(0)
01700   if sumjob$="N" then goto L1720
01710   gosub PRINTSUMMARYBYJOB
01720 L1720: if eofc=1 then goto L1760 else goto L720
01730 L1730: eofc=1
01740   goto L1540
01750 ! ______________________________________________________________________
01760 L1760: if sumjob$="N" or prtdet$="N" then goto L1790 ! END OF JOB ROUTINE
01770   if eofc=1 or eofc=2 then goto L1790
01780   gosub PRINTSUMMARYBYJOB
01790 L1790: eofc=2
01800   mat totjob=totall
01810   goto L1540
01820 L1820: if sumjob$="N" then goto L1840
01830   gosub PRINTSUMMARYOFALLJOBS
01840 L1840: close #1: 
01850   close #2: 
01860   close #3: 
01870 DONE: ! 
01880   fncloseprn
01890   goto XIT
01900 ! ______________________________________________________________________
01910 HDR: ! 
01920   if hd=1 then goto L1980
01930   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
01940   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
01950   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
01960   pr #255: "\qc  {\f181 \fs16 \b As of "&cnvrt$("pic(zz/zz/zz)",dat1)&"}"
01970   pr #255: "\ql   "
01980 L1980: if eofc=0 then pr #255,using L1990: "Job Number "&jn$,n$ !:
          ! pr SUB-HEADING
01990 L1990: form skip 1,pos 30,c 17,pos 65,c 40,skip 2
02000   pr #255,using L2010: "Category","Category","Reference","Labor","Other","Estimated Cost","Over/Under"
02010 L2010: form pos 1,c 8,pos 11,c 8,pos 21,c 9,pos 54,c 5,pos 68,c 5,pos 92,c 14,pos 124,c 10,skip 1
02020   pr #255,using L2030: "Number","Description","Number","Date","Hours","Cost","Cost","Sub-Category","Labor","Other","Labor","Other" pageoflow NWPGE
02030 L2030: form pos 1,c 6,pos 9,c 11,pos 22,c 6,pos 30,c 4,pos 40,c 5,pos 55,c 4,pos 69,c 4,pos 76,c 12,pos 91,c 5,pos 105,c 5,pos 119,c 5,pos 133,c 5,skip 2
02040   return 
02050 ! ______________________________________________________________________
02060 PRINTDETAILLINE: ! 
02070   if fstdet=1 then goto L2110
02080   fstdet=1
02090   pr #255,using L2100: tr(1),k$(1:20)
02100 L2100: form pos 1,n 5,pos 9,c 20,skip 1
02110 L2110: if tr(5)+tr(6)><0 then goto L2190
02120   if rtrm$(pd$)="" then goto L2150
02130   pr #255,using L2160: eno$,tr(4),0,0,tr(8)+tr(9),tr(2)," - "&pd$(1:25) pageoflow NWPGE
02140   goto L2160
02150 L2150: pr #255,using L2160: eno$,tr(4),0,0,tr(8)+tr(9),tr(2)," - "&desc$(tr(2))(1:25) pageoflow NWPGE
02160 L2160: form skip 1,pos 15,c 12,pos 28,pic(zz/zz/zz),pos 36,n 9.2,2*n 14.2,pos 74,n 4,c 28,skip 1
02170   tr89=tr89+tr(8)+tr(9)
02180   goto L2260
02190 L2190: if rtrm$(pd$)="" then goto L2220
02200   pr #255,using L2160: eno$,tr(4),tr(5)+tr(6),tr(9),tr(8),tr(2)," - "&pd$(1:25) pageoflow NWPGE
02210   goto L2230
02220 L2220: pr #255,using L2160: eno$,tr(4),tr(5)+tr(6),tr(9),tr(8),tr(2)," - "&desc$(tr(2))(1:25) pageoflow NWPGE
02230 L2230: tr56=tr56+tr(5)+tr(6)
02240   tr9=tr9+tr(9)
02250   tr8=tr8+tr(8)
02260 L2260: return 
02270 ! ______________________________________________________________________
02280 ACCUMULATEFORCATEGORYSUMMARY: ! 
02290   cattot(tr(2))=cattot(tr(2))+tr(8)+tr(9)
02300   cdesc$(tr(2))=desc$(tr(2))
02310   if lcat=0 and hcat=0 then goto L2320 else goto L2350
02320 L2320: lcat=tr(2)
02330   hcat=lcat
02340   goto L2370
02350 L2350: if tr(2)<lcat then lcat=tr(2)
02360   if tr(2)>hcat then hcat=tr(2)
02370 L2370: return 
02380 ! ______________________________________________________________________
02390 PRINTCATEGORYSUMMARY: ! 
02400   if sumcat$="N" then goto L2490 else pr #255,using L2410: "************** Summary by Category *************"
02410 L2410: form pos 90,cc 50,skip 1
02420 L2420: form pos 90,c 50,skip 2
02430   for j=lcat to hcat
02440     if cattot(j)=0 then goto L2470
02450     pr #255,using L2460: cdesc$(j),cattot(j) pageoflow NWPGE
02460 L2460: form pos 90,c 30,pos 124,n 14.2,skip 1
02470 L2470: next j
02480   pr #255,using L2420: "************************************************"
02490 L2490: mat jobtot=jobtot+cattot
02500   for j=lcat to hcat
02510     if rtrm$(cdesc$(j))="" then goto L2530
02520     jtot$(j)=cdesc$(j)
02530 L2530: next j
02540   mat cattot=(0)
02550   lcat=0
02560   hcat=0
02570   return 
02580 ! ______________________________________________________________________
02590 PRINTSUMMARYBYJOB: ! 
02600   if sumjob$="N" then goto L2690
02610   pr #255,using L2410: "***************** SUMMARY BY JOB ***************"
02620   form skip 1,pos 90,c 27,skip 2
02630   for j=1 to 100
02640     if jobtot(j)=0 then goto L2670
02650     pr #255,using L2660: jtot$(j),jobtot(j) pageoflow NWPGE
02660 L2660: form pos 90,c 30,pos 124,n 14.2,skip 1
02670 L2670: next j
02680   pr #255,using L2420: "************************************************"
02690 L2690: mat tottot=tottot+jobtot
02700   for j=1 to 100
02710     if rtrm$(jtot$(j))="" then goto L2730
02720     tottot$(j)=jtot$(j)
02730 L2730: next j
02740   mat jobtot=(0)
02750   return 
02760 ! ______________________________________________________________________
02770 TOTALLINEWITHOUTDETAILS: ! 
02780   pr #255,using L2790: val(cn$(7:11)),k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
02790 L2790: form pos 1,n 5,pos 9,c 20,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
02800   return 
02810 ! ______________________________________________________________________
02820 PRINTSUMMARYOFALLJOBS: ! 
02830   if sumcat$="N" and sumjob$="N" then goto L2920
02840   pr #255,using L2410: "*************** Summary of All Jobs ************"
02850   form skip 1,pos 90,c 27,skip 2
02860   for j=1 to 100
02870     if tottot(j)=0 then goto L2900
02880     pr #255,using L2890: tottot$(j),tottot(j) pageoflow NWPGE
02890 L2890: form pos 90,c 30,pos 124,n 14.2,skip 1
02900 L2900: next j
02910   pr #255,using L2420: "************************************************"
02920 L2920: return 
02930 ! ______________________________________________________________________
02940 NWPGE: ! 
02950   pr #255: newpage
02960   hd=0
02970   gosub HDR
02980   hd=1
02990   continue 
03000 ! ______________________________________________________________________
03010 XIT: fnxit
03020 ! ______________________________________________________________________
03030 ! <Updateable Region: ERTN>
03040 ERTN: fnerror(program$,err,line,act$,"xit")
03050   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03070   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
03080 ERTN_EXEC_ACT: execute act$ : goto ERTN
03090 ! /region
03100 ! ______________________________________________________________________
