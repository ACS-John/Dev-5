00010 ! Replace R:\acsGL\AcGlPpaj
00020 ! -- Enter Prior Period Adjustments
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fnxit,fntop, fnopenprn,fncloseprn,fnerror,fncno,fndat,fngl_number_use_dept,fngld2
00050   let fntop(program$,cap$="Prior Period Adjustments")
00060 ! ______________________________________________________________________
00070   on error goto ERTN
00080 ! ______________________________________________________________________
00090   dim d$*50,bc(13),bp(13),flo$(8),scr$(8)*20,shd$*60,sfl$(5),io1$(9)
00100   dim cnam$*40,dat$*20,fm(4) ,flo1$(4),cap$*128
00110 ! ______________________________________________________________________
00120   let fncno(cno,cnam$) !:
        let fndat(dat$)
00130   open #1: "Name=Q:\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative: read #1,using "Form pos 384,N 2",rec=1: nap : close #1: 
00140 ! ______________________________________________________________________
00150 ! 
00160 ! 
00170 ! ______________________________________________________________________
00180   open #1: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",KFName=Q:\GLmstr\GLIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00190   open #2: "Name=Q:\GLmstr\AcTrans.h"&str$(cno)&",KFName=Q:\GLmstr\AcTrIdx.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L210
00200   let ac1=1
00210 L210: if fngl_number_use_dept=1 then let io1$(1)="4,25,N 3,UT,N" else !:
          let io1$(1)="4,25,N 3,P,N": let scr$(1)=""
00220   let io1$(2)="4,29,N 6,UT,N"
00230   if fngld2=1 then let io1$(3)="4,36,N 3,UT,N" else !:
          let io1$(3)="4,36,N 3,P,N" : let scr$(3)=""
00240   let io1$(4)="6,25,Gz 12.2,UT,N" : let io1$(5)="08,25,G 6,UT,N" !:
        let io1$(6)="10,25,N 2,UT,N" : let io1$(7)="11,25,N 1,UT,N" !:
        let io1$(8)="13,25,N 2,UT,N" : let io1$(9)="14,25,N 1,UT,N"
00250   let flo1$(1)=io1$(6) : let flo1$(2)=io1$(7) !:
        let flo1$(3)=io1$(8) : let flo1$(4)=io1$(9)
00260   let fnopenprn
00270   gosub HDR
00280 L280: print newpage
00290   close #101: ioerr L300
00300 L300: open #101: "SROW=3,SCOL=18,EROW=17,ECOL=63,Border=DR,Caption=<"&cap$,display,outin 
00310   print #101: newpage
00320   let flo$(1)="4,3,C 15,N"
00330   print #101,fields "1,1,Cc 46,R,N": cnam$
00340   print #101,fields "2,1,Cc 46,R,N": "Company Number "&str$(cno)
00350   print #101,fields "04,02,Cr 22,N": "General Ledger Number:"
00360   print #101,fields "06,02,Cr 22,N": "Adjustment Amount:"
00370   print #101,fields "08,02,Cr 22,N": "Date:"
00380   print #101,fields "10,02,Cr 22,N": "First Month Affected:"
00390   print #101,fields "11,02,Cr 22,N": "First Year Affected:"
00400   print #101,fields "13,02,Cr 22,N": "Last Month Affected:"
00410   print #101,fields "14,02,Cr 22,N": "Last Year Affected:"
00420   print #101,fields "11,27,C 19,N": "(1=Current 2=Prior)"
00430   print #101,fields "14,27,C 19,N": "(1=Current 2=Prior)"
00440   print fields "18,30,C 09,B,1": "Next (F1)"
00450   print fields "18,41,C 09,B,5": "Exit (F5)"
00460 ! Print #101,Fields MAT IO1$: MAT FM ! using this will display next gl number when you loop back through
00470   let dno=ano=sno=am=0
00480 L480: rinput #101,fields mat io1$: dno,ano,sno,am,d1,mat fm conv CONV1
00490   let k$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
00500   if ce>0 then let io1$(ce)(ce1:ce2)="U": let ce=0
00510   if cmdkey>0 then goto L680 else let ce=curfld
00520   if ce><4 then goto L540
00530   print fields io1$(4): am conv ERR1
00540 L540: if ce><3 then goto L600
00550   let ce=2
00560   read #1,using L570,key=k$: d$ nokey ERR1
00570 L570: form pos 13,c 50
00580   print fields "5,14,C 50,RH,N": d$
00590   let ce=3
00600 L600: let ce=ce+1
00610   if ce>udim(io1$) then let ce=1
00620 L620: let io1$(ce)=rtrm$(uprc$(io1$(ce))): let ce1=pos(io1$(ce),"U",1)
00630   if ce1=0 then goto L540
00640   let ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L480
00650 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00660   let ce=cnt+1
00670 ERR1: print fields "24,78,C 1": bell : goto L620
00680 L680: if cmdkey=5 then goto L1060
00690   if am=0 then let ce=4: goto ERR1
00700   if d1<10100 or d1>123199 then let ce=5: goto ERR1
00710   if fm(1)<1 or fm(1)>13 then let ce=6: goto ERR1
00720   if fm(3)<1 or fm(3)>13 then let ce=8: goto ERR1
00730   if fm(2)<1 or fm(2)>2 then let ce=7: goto ERR1
00740   if fm(4)<1 or fm(4)>2 then let ce=9: goto ERR1
00750   let ce=2
00760   read #1,using L770,key=k$: d$,bb,cb,mat bc,mat bp nokey ERR1
00770 L770: form pos 13,c 50,pos 81,41*pd 6.2
00780   let ce=0
00790   if fm(2)=1 then let fm2$="C" else let fm2$="P"
00800   if fm(4)=1 then let fm4$="C" else let fm4$="P"
00810   print #255,using L820: dno,ano,sno,d$(1:35),fm(1),fm2$,fm(3),fm4$,am pageoflow PGOF
00820 L820: form pos 9,pic(zzz),x 6,pic(zzzzzz),x 9,pic(zzz),x 4,c 35,x 1,n 2,x 1,c 1,x 11,n 2,x 1,c 1,x 4,n 11.2,skip 1
00830   if am>0 then let am1=am1+am else let am2=am2+am
00840   goto L850
00850 L850: if fm(2)=1 then goto L930
00860   if fm(4)=1 then let last=nap else let last=fm(3)
00870   for j=fm(1) to last
00880     let bp(j)=bp(j)+am
00890   next j
00900   if fm(4)=2 then goto L990
00910   let first=1
00920   goto L940
00930 L930: let first =fm(1)
00940 L940: for j=first to fm(3)
00950     let bc(j)=bc(j)+am
00960   next j
00970   let bb=bb+am
00980   let cb=cb+am
00990 L990: rewrite #1,using L1000,key=k$: bb,cb,mat bc,mat bp
01000 L1000: form pos 81,41*pd 6.2
01010   if ac1=0 then goto L1050
01020   if fm(2)><1 then goto L1050 ! CURRENT YEAR ONLY
01030   write #2,using L1040: dno,ano,sno,d1,am,3,0,"PPAJ"&date$,"PRIOR PERIOD ADJUSTMENT",fm(1)
01040 L1040: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
01050 L1050: goto L280
01060 L1060: close #1: 
01070   close #2: ioerr L1080
01080 L1080: print newpage
01090   print #255,using L1100: "Total Debits: ",am1
01100 L1100: form pos 5,c 18,n 12.2
01110   print #255,using L1100: "Total Credits: ",am2
01120   print #255,using L1100: "Net Adjustments: ",am1+am2
01130   let fncloseprn
01140   execute "INDEX Q:\GLmstr\AcTrans.h"&str$(cno)&" Q:\GLmstr\AcTrIdx.h"&str$(cno)&" 1/71/17/13 12/2/2/4 Replace DupKeys"
01150   goto XIT
01160 HDR: ! 
01170   print #255,using L1180: cnam$,cap$,dat$
01180 L1180: form pos 20,cc 40,skip 1,pos 20,cc 40,skip 1,pos 20,cc 40,skip 2
01190   print #255,using L1200: scr$(1),scr$(2),scr$(3),"Description","1st Month/Yr   Last Month/Yr   Amount"
01200 L1200: form pos 4,c 12,x 2,c 12,c 13,pos 43,c 12,pos 73,c 60
01210   return 
01220 ! ______________________________________________________________________
01230 PGOF: ! 
01240   print #255: newpage
01250   gosub HDR
01260   continue 
01270 XIT: let fnxit
01280 ! ______________________________________________________________________
01290 ! <updateable region: ertn>
01300 ERTN: let fnerror(cap$,err,line,act$,"xit")
01310   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01320   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01330   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01340 ERTN_EXEC_ACT: execute act$ : goto ERTN
01350 ! /region
