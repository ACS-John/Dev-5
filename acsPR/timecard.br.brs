00010 !  Replace S:\acsPR\TimeCard
00020 ! Extend Time Card
00030 ! ______________________________________________________________________
00040   dim sc1$(7),fl1$(7),io1$(101),inp(20,5),hrs(20),em$*30
00050   dim p$(20)*50,wrd2$(2)*35,cap$*128,message$*40
00060 ! ______________________________________________________________________
00070   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fncloseprn,fnopenprn,fncno,fnerror,fntop,fnxit,fnconsole
00080   on error goto ERTN
00090 ! ______________________________________________________________________
00100   fntop("S:\acsPR\TimeCard",cap$="Extend Time Card")
00110   fncno(cno)
00115   fnconsole(1)
00120 ! 
00130 ! ______________________________________________________________________
00140   sc1$(1)="Employee #:": fl1$(1)="1,2,C 20,N"
00150   sc1$(2)=" Date": fl1$(2)="3,15,C 6,R,N"
00160   sc1$(3)="Time  In": fl1$(3)="2,28,C 8,R,N"
00170   sc1$(4)="Time Out": fl1$(4)="2,42,C 8,R,N"
00180   sc1$(5)="Hrs  Min": fl1$(5)="3,28,C 8,R,N"
00190   sc1$(6)="Hrs  Min": fl1$(6)="3,42,C 8,R,N"
00200   sc1$(7)=" Hours": fl1$(7)="3,55,C 6,R,N"
00210   let io1$(1)="1,21,N 9,UET,N"
00220   for j=2 to 21
00230     let io1$(j*5-8)=str$(j+2)&",15,N 6,UT,N"
00240     let io1$(j*5-7)=str$(j+2)&",28,N 2,UT,N"
00250     let io1$(j*5-6)=str$(j+2)&",34,N 2,UT,N"
00260     let io1$(j*5-5)=str$(j+2)&",42,N 2,UT,N"
00270     let io1$(j*5-4)=str$(j+2)&",48,N 2,UET,N"
00280   next j
00290   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&",Shr",internal,input,keyed 
00300   pr newpage !:
        fnopenwin(win=101,10,22,15,58,cap$)
00310   let io2$(1)="4,2,C 35,N"
00320   let io2$(2)="5,2,C 35,N"
00330   let wrd2$(1)="Use Hours and Minutes"
00340   let wrd2$(2)="Use Hours and Hundredths of an Hour"
00350   pr f "16,35,C 09,B,5": "Exit (F5)"
00360   rinput #win,select mat io2$,attr "H": mat wrd2$
00370   if cmdkey=5 then goto XIT
00380   let ti1=curfld
00390   if ti1=2 then sc1$(5)=sc1$(6)="Hrs  Hnd"
00400 L400: pr newpage
00410   pr f mat fl1$: mat sc1$
00420   pr f "24,11,C 09,B,1": "Next (F1)"
00430   pr f "24,21,C 10,B,2": "Print (F2)"
00440   pr f "24,32,C 26,B,3": "Skip to Next Employee (F3)"
00450   pr f "24,59,C 09,B,5": "Stop (F5)"
00460 L460: input fields mat io1$: eno,mat inp conv CONV1
00470   if ce>0 then let io1$(ce)(ce1:ce2)="U": ce=0
00480   if cmdkey>0 then goto L620 else ce=curfld
00490   if ce>1 then goto L540
00500   read #1,using L510,key=lpad$(str$(eno),8): em$ nokey ERR1
00510 L510: form pos 9,c 30
00520   pr f "1,35,C 30,R,N": em$
00530   goto L550
00540 L540: if int((ce-1)/5)=(ce-1)/5 then goto L650
00550 L550: ce=ce+1
00560   if ce>udim(io1$) then ce=1
00570 L570: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
00580   ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L460
00590 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00600   ce=cnt+1
00610 ERR1: pr f "24,78,C 1": bell : goto L570
00620 L620: if cmdkey=2 then goto L860
00630   if cmdkey=3 then goto L1080
00640   if cmdkey=5 or eno=0 then goto END1
00650 L650: ln=currow
00660   cn=curcol
00670   if ln=1 then goto L460
00680   if ti1=1 then let hrs(ln-3)=round((inp(ln-3,4)+(inp(ln-3,5)/60))-(inp(ln-3,2)+(inp(ln-3,3)/60)),2) else let hrs(ln-3)=inp(ln-3,4)+(inp(ln-3,5)*.01)-inp(ln-3,2)-(inp(ln-3,3)*.01)
00690   if hrs(ln-3)<0 then let hrs(ln-3)=hrs(ln-3)+12
00700   if inp(ln-3,2)<1 or inp(ln-3,2)>12 then ce=ln*5-17: goto ERR1
00710   if inp(ln-3,3)<0 or inp(ln-3,3)>59 then ce=ln*5-16: goto ERR1
00720   if inp(ln-3,4)<1 or inp(ln-3,4)>12 then ce=ln*5-15: goto ERR1
00730   if inp(ln-3,5)<0 or inp(ln-3,5)>59 then ce=ln*5-14: goto ERR1
00740   if inp(ln-3,1)<10101 or inp(ln-3,1)>123199 then ce=ln*5-18: goto ERR1
00750   if hrs(ln-3)>12 then ce=ln*5-15: goto ERR1
00760   pr f str$(ln)&",55,N 6.2,H,N": hrs(ln-3)
00770   ce=ln*5-12
00780   ce1=pos(io1$(ce),"U",1)
00790   ce2=ce1+1
00800   let io1$(ce)(ce1:ce1)="CU"
00810   if inp(ln-3,4)>2 and inp(ln-3,4)<8 then let dt=inp(ln-3,1)+100 else let dt=inp(ln-3,1)
00820   if ln<23 then pr f str$(ln+1)&",15,N 6,UT,N": dt
00830   pr f "2,55,N 6.2,R,N": sum(hrs)
00840   goto L460
00850 ! ______________________________________________________________________
00860 L860: let fnopenprn(cp,58,220,process)
00870   fnwait(104,cap$,message$,0)
00880   pr #255,using L890: mat sc1$
00890 L890: form pos 1,4*c 14,skip 1,pos 29,3*c 14
00900   let t1=t2=0
00910   for j=1 to 20
00920     if j>1 then eno=0
00930     if hrs(j)=0 then goto L1030
00940     pr #255,using L950: eno,inp(j,1),inp(j,2),inp(j,3),inp(j,4),inp(j,5),hrs(j)
00950 L950: form pos 1,pic(zzzzzz),x 7,pic(zz/zz/zz),x 5,2*n 5,x 4,2*n 5,n 12.2,skip 1
00960     let t1=t1+hrs(j)
00970     if j=20 then goto L990
00980     if inp(j,1)=inp(j+1,1) then goto L1030
00990 L990: pr #255,using L1000: "__________",t1
01000 L1000: form pos 53,c 10,skip 1,pos 53,n 10.2,skip 2
01010     let t2=t2+t1
01020     let t1=0
01030 L1030: next j
01040   pr #255,using L1050: "__________",t2,"=========="
01050 L1050: form pos 53,c 10,skip 1,pos 53,n 10.2,skip 1,pos 53,c 10,skip 2
01060   pr #255: newpage
01070   fncloseprn
01080 L1080: mat hrs=(0)
01090   mat inp=(0)
01100   goto L400
01110 ! ______________________________________________________________________
01120 END1: let fncloseprn
01130 XIT: let fnxit
01140 ! ______________________________________________________________________
01150 ! <Updateable Region: ERTN>
01160 ERTN: let fnerror(program$,err,line,act$,"xit")
01170   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01180   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01190   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01200 ERTN_EXEC_ACT: execute act$ : goto ERTN
01210 ! /region
01220 ! ______________________________________________________________________
