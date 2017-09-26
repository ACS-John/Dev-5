00010 ! Replace S:\acsPR\JCCPR2
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fncno,fnerror
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim em$(3)*30,ss$*11,jn$*6,tr(9),en$*8,tdet(3),n$*40,tcp(22)
00070   dim tded(6),tdc(5),cnam$*40,dedcode(10),a2$*70,cap$*128,message$*40
00080   dim jn1$*6,tr1(9),en1$*8,pl1$(6)*30,pl2$(6)*6,dr(7),hr1(8),hr2(8),ded(6)
00090 ! ______________________________________________________________________
00100   let fncno(cno,cnam$)
00110 ! 
00120   let cap$="Certified Payroll Register"
00130 ! ______________________________________________________________________
00140   print newpage
00150   on fkey 5 goto L1930
00160   let message$="Printing: please wait..."
00170   let fnwait(101,cap$,message$,1)
00180   let fnopenprn(cp,58,220,process)
00190   if file$(255)(1:3)<>"PRN" then let jbskip=1
00200 ! ______________________________________________________________________
00210   let pl2$(1)="FICA"
00220   let pl2$(2)="Fed"
00230   let pl2$(3)="State"
00240   let pl2$(4)="Union"
00250   let pl2$(5)="Other"
00260 ! ______________________________________________________________________
00270   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #1,using 'Form POS 618,10*N 1,POS 758,N 2',rec=1: mat dedcode,un !:
        close #1: 
00280 ! ______________________________________________________________________
00290   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&",Shr",internal,input,keyed 
00300   open #2: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00310   open #3: "Name=Work."&session$,internal,input,relative 
00320   open #4: "Name="&env$('Temp')&"\Addr."&session$,internal,input 
00330   open #5: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&str$(cno)&",Shr",internal,input,relative 
00340   read #3,using L350,rec=1: df,dt,mat dr
00350 L350: form pos 1,2*n 6,7*pd 3
00360 L360: read #4,using L370: r4 eof L1890
00370 L370: form pos 1,pd 3
00380   if r4=1 then goto L360
00390   read #3,using L400,rec=r4: en1$,jn1$,mat tr1
00400 L400: form pos 5,c 8,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2
00410   if jn1$><jn$ then goto L580
00420   if en1$><en$ then goto L700
00430   if tr1(3)><tr(3) then goto L750
00440 L440: for j=1 to 7
00450     if int(tr1(4)*.01)=dr(j) then goto L490
00460   next j
00470   goto L360
00480 ! ______________________________________________________________________
00490 L490: let hr1(j)=hr1(j)+tr1(5)
00500   let hr1(8)=hr1(8)+tr1(5)
00510   let hr2(j)=hr2(j)+tr1(6)
00520   let hr2(8)=hr2(8)+tr1(6)
00530   let gp1=gp1+tr1(5)*tdet2
00540   let gp2=gp2+tr1(6)*tdet3
00550   let tr(3)=tr1(3)
00560   goto L360
00570 ! ______________________________________________________________________
00580 L580: if rtrm$(jn$)="" then goto L620
00590   gosub L1550
00600   gosub TOTALS
00610   print #255: newpage
00620 L620: let en$=en1$
00630   let jn$=jn1$
00640   let n$=""
00650   read #2,using L660,key=jn$: n$ nokey L670
00660 L660: form pos 7,c 40
00670 L670: if end4=0 then gosub L1240
00680   goto L780
00690 ! ______________________________________________________________________
00700 L700: if val(en$)=0 then goto MOVEINFO
00710   gosub L1550
00720   let en$=en1$
00730   goto L780
00740 ! ______________________________________________________________________
00750 L750: gosub L1390
00760   goto L800
00770 ! ______________________________________________________________________
00780 L780: read #1,using L790,key=en$: mat em$,ss$,em2,lpd,tgp,ta1 nokey L1060
00790 L790: form pos 9,3*c 30,c 11,x 4,n 2,pos 162,n 6,pd 5.2,pd 3
00800 L800: let adr=ta1
00810   mat ded=(0)
00820   let tdet2=0
00830   let tgp=tdet3=0
00840 L840: if adr=0 then goto MOVEINFO
00850   read #5,using L860,rec=adr: tdn,tdt4,mat tdet,mat tdc,mat tcp,adr
00860 L860: form pos 9,n 3,pos 42,n 6,pos 58,3*pd 4.2,pos 150,5*pd 3.2,pos 358,22*pd 5.2,pd 3
00870   if tdn><tr1(3) then goto L900
00880   let tdet2=tdet(2)
00890   let tdet3=tdet(3)
00900 L900: if lpd><tdt4 then goto L1040
00910   let tgp=tgp+tcp(21)
00920   for j=1 to 5
00930     let tcd1=tcd1+tdc(j)
00940   next j
00950   let ded(1)=ded(1)+tcp(2)+tcp(15)
00960   let ded(2)=ded(2)+tcp(1)
00970   let ded(3)=ded(3)+tcp(3)
00980   if un>0 and un<11 then let ded(4)=ded(4)+tcp(un+3)
00990   for j=1 to 10
01000     if j=un then goto L1020
01010     if dedcode(j)=2 then let ded(5)=ded(5)-tcp(j+3) else let ded(5)=ded(5)+tcp(j+3)
01020 L1020: next j
01030   let tcp22=tcp22+tcp(22)
01040 L1040: goto L840
01050 ! ______________________________________________________________________
01060 L1060: mat em$=("")
01070   let ss$=""
01080   let em2=0
01090   let ta1=0
01100 MOVEINFO: ! 
01110   let pl1$(1)=em$(1)
01120   let pl1$(2)=em$(2)
01130   let pl1$(3)=em$(3)
01140   let pl1$(4)=ss$
01150 ! LET PL1$(5)=LPAD$(STR$(TDN),6)
01160   let pl1$(5)=lpad$(str$(em2),6)
01170   goto L440
01180 ! ______________________________________________________________________
01190 PGOF: ! 
01200   print #255: newpage
01210   gosub L1240
01220   continue 
01230 ! ______________________________________________________________________
01240 L1240: let p1=59-int(len(rtrm$(cnam$)))/2
01250   let a2$="Job # "&ltrm$(jn$)&"  Job Name "&rtrm$(n$)
01260   let p2=59-int(len(rtrm$(a2$)))/2
01270   print #255,using L1280: cnam$,a2$
01280 L1280: form skip 2,pos p1,c 70,skip 1,pos p2,c 70,skip 1
01290   print #255: tab(40);"****  Certified Payroll Register  ****"
01300   print #255,using L1310: "Period Ending",dt
01310 L1310: form pos 48,c 14,pic(zz/zz/zz),skip 1
01320   print #255: "Name  &  Address"
01330   print #255: "City, State Zip                <-------- Hours Worked this Job --------> Total  Pay  Pay  <--------------- Summary ---------------->"
01340   print #255,using L1350: "    Fed-Exempt",mat dr,"Hours  Rate Typ      Gross    FICA Fed W/H   Other      Net"
01350 L1350: form pos 1,c 30,pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),x 1,c 59,skip jbskip
01360   print #255: "______________________________ _____ _____ _____ _____ _____ _____ _____ _____  ____ ___    _______  ______  ______   _____  _______"
01370   return 
01380 ! ______________________________________________________________________
01390 L1390: if tgp=0 then let x3=0 else let x3=(gp1+gp2)/tgp
01400   if hr1(8)=0 then goto L1450
01410   let lnp=lnp+1
01420   if lnp>5 then let lnp=6
01430   print #255,using L1440: pl1$(lnp),mat hr1,tdet2," REG",gp1+gp2,x3*ded(1),x3*ded(2),x3*(ded(3)+ded(4)+ded(5)),gp1+gp2-(x3*ded(1)+x3*ded(2)+x3*(ded(3)+ded(4)+ded(5))) pageoflow PGOF
01440 L1440: form pos 1,c 30,9*n 6.2,c 6,n 9.2,3*n 8.2,n 9.2
01450 L1450: if hr2(8)=0 then goto L1500
01460   let lnp=lnp+1
01470   if lnp>5 then let lnp=6
01480   if hr1(8)=0 then print #255,using L1440: pl1$(lnp),mat hr2,tdet3," OVT",gp1+gp2,x3*ded(1),x3*ded(2),x3*(ded(3)+ded(4)+ded(5)),gp1+gp2-(x3*ded(1)+x3*ded(2)+x3*(ded(3)+ded(4)+ded(5))) pageoflow PGOF else print #255,using L1490: pl1$(lnp),mat hr2,tdet3," OVT" pageoflow PGOF
01490 L1490: form pos 1,c 30,9*n 6.2,c 6
01500 L1500: let hr8=hr8+hr1(8)+hr2(8)
01510   mat hr1=(0)
01520   mat hr2=(0)
01530   return 
01540 ! ______________________________________________________________________
01550 L1550: gosub L1390
01560   let lnp=lnp+1
01570   if lnp>5 then goto L1620
01580   for j=lnp to 5
01590     print #255,using L1600: pl1$(j) pageoflow PGOF
01600 L1600: form pos 1,c 30,skip 1
01610   next j
01620 L1620: let gded= gded+(x3*ded(1)+x3*ded(2)+x3*(ded(3)+ded(4)+ded(5)))
01630   let jgp=jgp+gp1+gp2 ! TOTAL GROSS FOR JOB
01640   mat tded=tded+ded ! TOTAL DEDUCTION
01650   let tcdt=tcdt+tcd1 ! TOTAL HOURS FOR ALL JOBS
01660   let thr=thr+hr8 ! TOTAL HOURS FOR JOB
01670   let tgp=0
01680   let gp1=0
01690   let gp2=0
01700   mat ded=(0)
01710   let tcd1=0
01720   let hr8=0
01730   let tcp22=0
01740   let lnp=0
01750   return 
01760 ! ______________________________________________________________________
01770 TOTALS: ! 
01780   print #255,using L1790: "* * * *  Totals for Job # ",jn$,"* * * *" pageoflow PGOF
01790 L1790: form pos 10,c 26,c 7,c 8,skip 1
01800   print #255: tab(6);"Total Hours   Gross Pay     Total        Total" pageoflow PGOF
01810   print #255: tab(31);"Deductions     Net-Pay" pageoflow PGOF
01820   print #255,using L1830: thr,jgp,gded,jgp-gded pageoflow PGOF
01830 L1830: form pos 5,4*n 12.2,skip 2
01840   let thr=0
01850   let jgp=0
01860   let gded=0
01870   return 
01880 ! ______________________________________________________________________
01890 L1890: gosub L1550
01900   gosub TOTALS
01910   close #1: 
01920   close #5: 
01930 L1930: let fncloseprn
01940   goto XIT
01950 ! ______________________________________________________________________
01960 ! <Updateable Region: ERTN>
01970 ERTN: let fnerror(program$,err,line,act$,"xit")
01980   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01990   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02000   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
02010 ERTN_EXEC_ACT: execute act$ : goto ERTN
02020 ! /region
02030 ! ______________________________________________________________________
02040 XIT: let fnxit
02050 ! ______________________________________________________________________
