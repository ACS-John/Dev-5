00010 ! formerly S:\acsGL\acglTB
00020 ! pr Trial Balance
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim d$*50,tr(7),tr$*12,td$*30,n$*12,t$*12,x$*3
00080   dim a$(9)*3,cogl$(2)*12,u$*12,d(2),ta(2)
00090   dim resp$(10)*80
00100 ! ______________________________________________________________________
00110   let right=1
00120   fntop(program$,cap$="Trial Balance")
00150   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno'),internal,input,relative 
00151   read #20,using 'Form POS 150,2*N 1',rec=1: d(1),d(2) 
00152   read #20,using 'Form POS 152,2*C 12',rec=1: mat cogl$ 
00153   close #20: 
00160   a$(1)="C/D" : a$(2)="C/R" : a$(3)="ADJ" 
00162   a$(4)="A/P" : a$(5)="PR" : a$(6)="A/R" 
00164   a$(7)="S/J" : a$(8)="P/J" : a$(9)=" "
00170   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.H"&env$('cno')&",Shr",internal,input,keyed 
00180   open #2: "Name="&env$('Q')&"\GLmstr\GLTrans.H"&env$('cno')&",Shr",internal,input,relative 
00190   if fnprocess=1 then goto START_REPORT
00200 SCREEN1: ! 
00210   fntos(sn$="GLTB") 
00220   let lc=0 : let mylen=25 : let mypos=mylen+2
00230   fnchk(lc+=1,mypos,"List All Details",right) 
00240   let resp$(1)="True"
00250   fnlbl(lc+=1,1,"Cost Center:",mylen,right)
00260   fntxt(lc,mypos,5,0,0,'number') 
00270   let resp$(2)=""
00280   fnchk(lc+=1,mypos,"Subtotal after each fund",right) 
00290   let resp$(3)="True"
00300   fnlbl(lc+=1,1,"Starting Account:",mylen,right)
00310   fnqgl(lc,mypos,0,1) 
00320   let resp$(4)="[All]"
00330   fnlbl(lc+=1,1,"Ending Account:",mylen,right)
00340   fnqgl(lc,mypos,0,1) 
00350   let resp$(5)="[All]"
00360   fncmdset(3)
00370   fnacs(sn$,0,mat resp$,ckey)
00380   if ckey=5 then goto XIT
00390   if resp$(1)="True" then let pt=0 else let pt=1
00400   costcent=val(resp$(2)) 
00410   let n$=lpad$(str$(costcent),3)&"     0  0"
00420   if resp$(3)="True" then let subt=1 else let subt=0
00430   let sl1$=fnagl$(resp$(4)) 
00440   let sl2$=fnagl$(resp$(5))
00600   restore #1,key>=n$: nokey SCREEN1
00610   if fnprocess=0 then 
00612     restore #1,key>=sl1$: nokey ignore
00614   end if
00620 START_REPORT: ! r:
00630   on fkey 5 goto L950
00640   fnopenprn
00650   gosub HDR2
00660 ! IF D(1)=0 OR fnPROCESS=1 THEN GOTO 310 ELSE GOTO 340
00670 READ_1: ! 
00680   read #1,using L690: n$,d$,bb,cb,mat ta eof L940
00690   L690: form pos 1,c 12,c 50,pos 81,2*pd 6.2,pos 333,2*pd 3
00700   let olddno=dno
00710   if costcent><0 and val(n$(1:3))><costcent then goto READ_1
00720   if sgl(4)=0 and sgl(5)=0 and sgl(6)=0 then goto L740
00730   if n$>sl2$ then goto L950
00740   L740: let dno=val(n$(1:3))
00750   if subt=1 and olddno>0 and olddno<>dno then pr #255,using L760: "FUND "&str$(olddno)&" TOTALS",fundt1,fundt2,fundt3 else goto L780
00760   L760: form skip 1,pos 30,c 20,pos 80,pic(zz,zzz,zzz.## cr),pic(z,zzz,zzz.## cr),pic(zz,zzz,zzz.## cr),skip 2
00770   let fundt1=fundt2=fundt3=0
00780   L780: ano=val(n$(4:9))
00790   let sno=val(n$(10:12))
00800   begbal=begbal+bb
00810   curbal=curbal+cb
00820   let fundt1=fundt1+bb
00830   let fundt3=fundt3+cb
00840   gosub L1200
00850   if ta(1)=0 then goto READ_1
00860   adr=ta(1)
00870   do until adr=0 
00880     read #2,using L890,rec=adr: t$,tr(4),tr(5),tr(6),tr(7),tr$,td$,adr
00890     L890: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
00900     gosub L1290
00910   loop
00920   gosub L1520
00930   goto READ_1
00940 L940: let olddno=dno
00950 L950: pr #255: 
00960   if subt=1 then 
00962     pr #255,using L760: "Fund "&str$(olddno)&" Totals",fundt1,fundt2,fundt3
00964   end if
00970   pr #255: ,"Trial Balance Proof Totals";
00980   pr #255,using L990: begbal,trtotal,curbal
00990   L990: form pos 80,pic(zz,zzz,zzz.## cr),pic(z,zzz,zzz.## cr),pic(zz,zzz,zzz.## cr)
01000   close #1: ioerr ignore
01010   close #2: ioerr ignore
01020   fncloseprn
01030 ! 
01040   goto XIT ! /r
01050 ! ______________________________________________________________________
01060 HDR2: ! r:
01070   pr #255,using L1090: date$('mm/dd/yy'),env$('cnam')
01080   pr #255,using L1090: time$,cap$
01090   L1090: form pos 1,c 8,pos 15,cc 50
01100   pr #255,using L1110: fnpedat$,"Page ",p1+=1
01110   L1110: form pos 15,cc 50,pos 115,c 5,n 4,skip 2
01120   pr #255,using L1130: "Account","Reference","Beginning","Current","Ending"
01130   L1130: form pos 6,c 7,pos 70,c 9,pos 84,c 9,pos 99,c 7,pos 116,c 6,skip 1
01140   pr #255,using L1150: "Number","Account Name/Transaction Description","Date  Source","Number","Balance","Activity","Balance"
01150   L1150: form pos 6,c 6,pos 17,c 36,pos 54,c 13,pos 71,c 6,pos 85,c 7,pos 99,c 8,pos 116,c 7
01160   pr #255,using L1170: "__________","____________________________________","____","______","___________","_________","__________","_________"
01170   L1170: form pos 4,c 10,pos 17,c 36,pos 54,c 4,pos 60,c 6,pos 69,c 11,pos 84,c 9,pos 98,c 10,pos 115,c 10,skip 2
01180 return ! /r
01200 L1200: ! r:
01202 if ta(1)=0 then 
01204     if pt=0 then 
01206       pr #255,using L1220: dno,ano,sno,d$,bb,cb pageoflow PGOF
01208       L1220: form pos 1,pic(---),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(zz,zzz,zzz.## cr),pos 111,pic(zz,zzz,zzz.## cr),skip 2
01210     end if
01212   else 
01214     if pt=0 then 
01216       pr #255,using L1250: dno,ano,sno,d$,bb pageoflow PGOF
01218       L1250: form pos 1,pic(---),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(zz,zzz,zzz.## cr)
01220     end if
01222   end if 
01224 return ! /r
01280 L1290: ! r:
01290   if tr(6)<1 or tr(6)>9 then let x$="" else let x$=a$(tr(6))
01300   if val(cogl$(1)(4:9))=0 or val(cogl$(2)(4:9))=0 then goto L1360
01310   if t$>=cogl$(1) and t$<=cogl$(2) then goto L1320 else goto L1360
01320   L1320: !
01322   if tr(5)>0 then goto L1360
01330   let u0+=tr(5) : let trtotal+=tr(5): let fundt2+=tr(5) : let u$=t$
01340   goto L1500
01360   L1360: ! 
01362   if tr$="999999999999" then let tr$=" "
01364   let rn=73-int(len(ltrm$(tr$))/2)
01366   if tr(5)<0 then let tcr1+=tr(5) else let tdr1+=tr(5)
01368   if adr=0 and u0=0 then goto L1400 else goto L1460
01370   L1400: !
01372   if pt=0 then 
01374     pr #255,using L1410: td$,tr(4),x$,ltrm$(tr$),tr(5),cb pageoflow PGOF
01376   end if
01378   L1410: form pos 21,c 30,pos 52,pic(zz/zz/zz),pos 62,c 3,pos rn,c 12,pos 95,pic(zz,zzz,zzz.## cr),pos 111,pic(zz,zzz,zzz.## cr),skip 2
01380   ! pr #255,Using 'form pos 40,2*c 35,skip 2': "Total Debits: "&LTRM$(CNVRT$("PIC(ZZZZ,ZZZ,ZZZ,ZZ#.##)",TDR1)),"Total Credits: "&LTRM$(CNVRT$("PIC(ZZZZ,ZZZ,ZZZ,ZZ#.##)",ABS(TCR1)))
01382   let tdr1=tcr1=0
01384   goto L1480
01386   L1460: !
01388   if pt=0 then 
01390     pr #255,using L1470: td$,tr(4),x$,ltrm$(tr$),tr(5) pageoflow PGOF
01392   end if
01394   L1470: form pos 21,c 30,pos 52,pic(zz/zz/zz),pos 62,c 3,pos rn,c 12,pos 95,pic(zz,zzz,zzz.## cr)
01396   L1480: !
01398   let trtotal+=tr(5) : let fundt2+=tr(5)
01400   let u$=t$
01402   L1500: ! 
01404 return ! /r
01520 L1520: ! r:
01522   if u0=0 then goto L1570
01530   if u$<cogl$(1) or u$>cogl$(2) then goto L1570
01540   if pt=0 then pr #255,using L1550: "Summary Transaction",u0,cb pageoflow PGOF
01550   L1550: form pos 21,c 30,pos 95,pic(zz,zzz,zzz.## cr),pos 111,pic(zz,zzz,zzz.## cr),skip 2
01560   let u0=0
01570   L1570:  !
01580 return ! /r
01590 PGOF: ! r:
01592   pr #255: newpage
01600   gosub HDR2
01610 continue ! /r
01670 XIT: let fnxit
01690 ! <Updateable Region: ERTN>
01700 ERTN: let fnerror(program$,err,line,act$,"xit")
01710   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01720   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01730   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01740 ERTN_EXEC_ACT: execute act$ : goto ERTN
01750 ! /region
