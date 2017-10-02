00010 ! Replace S:\acsPR\W2Box16
00020 ! W-2 Box 12 Information File !:
        ! .   ! NOTE: AVOID CHANGING DATA FORMAT OF THIS FILE SO IT CAN MATCH UP !:
        ! .   ! WITH IT'S 39 COUNTERPART WHICH DOESN'T HAVE ANY LIBRARIES
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fndat,fnprocess,fnwait,fnoldmsgbox,fnopenprn,fncloseprn,fnxit,fntop,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim sc1$(5),io1$(30),fl1$(5),in1$(30)
00080   dim sc1$(5)*24,fl1$(5),hd$(2)*78,sc2$(9)*24,fl2$(5),io2$(9)*26
00090   dim t$*8,desc$(2)*12,amt(2),wrd1$(5)*35,cap$*128,message$*40
00100   dim response$(5)*1,msgline$(2)*60,dat$*20,cnam$*40,k$*8,hk$*8,fm1$*255
00110 ! ______________________________________________________________________
00120   let fntop("S:\acsPR\w2Box16",cap$="W-2 Supplemental Information")
00130   let fncno(cno,cnam$) !:
        let fndat(dat$)
00135   let fnconsole(1)
00150   let fm1$="Form  Pos 1,C 8"&rpt$(",C 12,G 10.2,3*G 1",6)
00160 ! 
00170   let io1$(1)="7,20,C 12,UT,N"
00180   let io1$(2)="7,34,G 10.2,UT,N"
00190   let io1$(3)="7,46,G 1,UT,N"
00200   let io1$(4)="7,52,G 1,UT,N"
00210   let io1$(5)="7,58,G 1,UT,N"
00220   for j=1 to 5
00230     let io1$(j+05)="08"&io1$(j)(2:18)
00240     let io1$(j+10)="10"&io1$(j)(2:18)
00250     let io1$(j+15)="11"&io1$(j)(2:18)
00260     let io1$(j+20)="12"&io1$(j)(2:18)
00270     let io1$(j+25)="13"&io1$(j)(2:18)
00280   next j
00290   open #1: "Name="&env$('Q')&"\PRmstr\prCode.h"&str$(cno)&",Shr",internal,input ioerr L300 !:
        read #1,using 'Form POS 5,N 5': ckno !:
        close #1: 
00300 L300: open #1: "Name="&env$('Q')&"\PRmstr\W2Box16.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\W2Index.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L2430
00310   let hd$(1)="    Add W-2 Special Info"
00320   let hd$(2)=" Maintaining W-2 Special Info"
00330 ! ______________________________________________________________________
00340 MENU1: pr newpage
00350   if fnprocess=1 then let ti=4 : goto L520
00360   let win=101
00370   let sr=6 : let sc=20 : let er=14 : let ec=59
00380   gosub XNOPENWIN
00390   for j=1 to 5
00400     let io3$(j)=str$(j+3)&",2,C 38,N"
00410   next j
00420   let io3$(3)="6,2,C 38,C,N"
00430   let wrd1$(1)="1. Initial File Preparation"
00440   let wrd1$(2)="2. Add"
00450   let wrd1$(3)="3. Edit"
00460   let wrd1$(4)="4. pr Proof List"
00470   let wrd1$(5)="5. Reorganize"
00480   pr fields "15,35,C 09,B,5": "Exit (F5)"
00490   rinput #win,select mat io3$,attr "H": mat wrd1$
00500   let ti=curfld
00510   if ti=0 or cmdkey=5 then goto L540
00520 L520: on ti goto L1470,L580,L590,L1800,L2370 none MENU1
00530 ! ______________________________________________________________________
00540 L540: close #1: 
00550   if new1=1 or cont=1 then goto L1690
00560 XIT: let fnxit
00570 ! ______________________________________________________________________
00580 L580: let new1=1
00590 L590: pr newpage
00600   close #101: ioerr L610
00610 L610: open #101: "SROW=2,SCOL=10,EROW=17,ECOL=65,Border=DR,Caption=<"&cap$,display,outin 
00620   pr fields "3,10,C 20": "Employee #:"
00630   if ce>0 then goto L820
00640 L640: input fields "3,22,C 8,UET,N": t$ conv L640
00650   if rtrm$(t$)="" or ltrm$(rtrm$(t$))="0" then goto MENU1
00660   let hk$=t$=uprc$(lpad$(rtrm$(t$),8))
00670   read #1,using fm1$,key=hk$: t$,mat in1$ nokey L690
00680   if ti=2 then goto L640 else goto L700
00690 L690: if ti=3 then goto L640
00700 L700: pr fields "16,20,C 45": "Effect on wages (0=None, 1=Add, 2=Subtract)"
00710   pr fields "6,20,C 45": "Description     Amount   Fed  Fica  State"
00720   pr fields "7,11,Cr 8,N": "Box 11:"
00730   pr fields "8,11,Cr 8,N": "Unused:"
00740   pr fields "10,11,Cr 8,N": "Box 12a:"
00750   pr fields "11,11,Cr 8,N": "Box 12b:"
00760   pr fields "12,11,Cr 8,N": "Box 12c:"
00770   pr fields "13,11,Cr 8,N": "Box 12d:"
00780   pr fields "18,20,C 9,B,1": "Next (F1)"
00790   pr fields "18,31,C 11,B,2": "Delete (F2)"
00800   pr fields "18,44,C 9,B,5": "Exit (F5)"
00810   if ti=2 then goto L830
00820 L820: pr fields mat io1$: mat in1$
00830 L830: input fields mat io1$: mat in1$ conv CONV1
00840   if ce>0 then let io1$(ce)(ce1:ce2)="U" : ce=0
00850   goto L930 ! IF CMDKEY>0 THEN GOTO 619 ELSE cE=CURFLD+1
00860 ! __________________________________________
00870   if ce>udim(io1$) then ce=1
00880 L880: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
00890   ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L830
00900 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00910   ce=cnt+1
00920 ERR1: pr fields "24,78,C 1": bell : goto L880
00930 L930: if cmdkey=2 then let t$="" : goto L1040
00940   if cmdkey=5 then goto MENU1
00950   for j=1 to 6
00960     ce=j*5-3
00970     let in1$(ce)=cnvrt$("N 10.2",val(in1$(ce))) conv ERR1
00980     for j1=1 to 3
00990       ce=j*5-3+j1
01000       let in1$(ce)=str$(val(in1$(ce))) conv ERR1
01010       if in1$(ce)<"0" or in1$(ce)>"2" then goto ERR1
01020     next j1
01030   next j
01040 L1040: ce=0
01050   if rtrm$(ltrm$(t$))="0" and ti=2 then goto MENU1
01060   if rtrm$(t$)="" and ti=2 then goto MENU1
01070   let k$=t$=uprc$(lpad$(rtrm$(t$),8))
01080   if rtrm$(t$)="" or ltrm$(rtrm$(t$))="0" then goto L1150
01090   if hk$=k$ then goto L1400
01100   if ti=2 then goto L1130
01110   goto L1150
01120 ! ______________________________________________________________________
01130 L1130: read #1,using fm1$,key=k$: k$ nokey L1430
01140   goto L1400
01150 L1150: if rtrm$(k$)="" or ltrm$(rtrm$(k$))="0" then goto L1160 else goto L1220
01160 L1160: let mtype=2
01170   let msgline$(1)="Delete this record (Y/N)"
01180   let fnoldmsgbox(mat response$,cap$,mat msgline$,mtype)
01190   if uprc$(response$(1))="Y" then let d1=1 else let d1=0
01200   goto L1280
01210 ! ______________________________________________________________________
01220 L1220: read #1,using fm1$,key=k$: k$ nokey L1240
01230   goto L590
01240 L1240: if ltrm$(k$)<>"0" then pr fields "24,2,C 50,R,N": " ENTER 1 TO CHANGE EMPLOYEE # OR 0 TO CORRECT: "
01250   pr fields "24,2,C 1": bell
01260 L1260: input fields "24,60,N 1,UET,N": d1 conv L1260
01270   pr fields "24,1,C 80,N": ""
01280 L1280: if d1=1 then goto L1320
01290   if d1<>1 then let t$=k$=hk$
01300   goto L820
01310 ! ______________________________________________________________________
01320 L1320: delete #1,key=hk$: nokey L1330
01330 L1330: if rtrm$(k$)="" or rtrm$(ltrm$(k$))="0" then goto L1370
01340   goto L1430
01350 ! ______________________________________________________________________
01360   rewrite #1,using fm1$: t$,mat in1$
01370 L1370: let new1=1
01380   goto L590
01390 ! ______________________________________________________________________
01400 L1400: rewrite #1,using fm1$,key=k$: t$,mat in1$ nokey L1430
01410   goto L590
01420 ! ______________________________________________________________________
01430 L1430: write #1,using fm1$: t$,mat in1$
01440   let new1=1
01450   goto L590
01460 ! ______________________________________________________________________
01470 L1470: pr newpage
01480   close #101: ioerr L1490
01490 L1490: open #101: "SROW=5,SCOL=13,EROW=15,ECOL=64,BORDER=DR,CAPTION=<INITIAL FILE PREPARATION",display,outin 
01500   pr fields "5,13,Cc 52,R,N": cnam$
01510   pr fields "6,13,Cc 52,R,N": "COMPANY NUMBER "&str$(cno)
01520   pr fields "8,13,C 52,R,N": " ******************   WARNING   ******************"
01530   pr fields "10,13,C 52,N": "  This selection will destroy all existing records"
01540   pr fields "11,13,C 52,N": "  in the W-2 Box 12 File."
01550   pr fields "16,33,C 11,B,5": "Cancel (F5)"
01560   pr fields "13,15,C 47,R,N": " Enter ERASE to destroy all records in"
01570   pr fields "14,15,C 47,R,N": " the W-2 Box 12 file:"
01580   input fields "14,40,Cu 5,UT,N": pas$
01590   if cmdkey=5 then goto MENU1
01600   if uprc$(pas$)="ERASE" then cont=1 else cont=2
01610   on cont goto L1630,MENU1 none L1470
01620 ! ______________________________________________________________________
01630 L1630: close #1: ioerr L1640
01640 L1640: open #1: "Name="&env$('Q')&"\PRmstr\W2Box16.h"&str$(cno),internal,output ioerr L1650
01650 L1650: close #1,free: ioerr L1660
01660 L1660: open #1: "Name="&env$('Q')&"\PRmstr\W2Box16.h"&str$(cno)&",Size=0,RecL=158,Replace",internal,output 
01670 L1670: cont=1
01680   close #1: ioerr L1690
01690 L1690: open #99: "Name=Proc."&wsid$&",Size=0,Replace",display,output ioerr L1710
01700   goto L1720
01710 L1710: open #99: "Name=Proc."&wsid$&",Size=0",display,output 
01720 L1720: pr #99: "CLEAR"
01730   pr #99: "PROCERR RETURN"
01740   pr #99: "Index "&env$('Q')&"\PRmstr\W2Box16.h"&str$(cno)&","&env$('Q')&"\PRmstr\W2Index.h"&str$(cno)&",1,8,Replace,DupKeys"
01750   if cont=1 then pr #99: "LOAD S:\acsPR\W2Box16" else pr #99: "LOAD menu"
01760   pr #99: "RUN"
01770   close #99: 
01780   chain "PROC=PROC."&wsid$
01790 ! ______________________________________________________________________
01800 L1800: pr newpage
01810   close #101: ioerr L1820
01820 L1820: open #101: "SROW=9,SCOL=10,EROW=11,ECOL=65,Border=DR,Caption=<"&cap$& " - Proof List",display,outin 
01830   pr fields "10,12,C 50": "Employee # to Start With (blank for all):"
01840   pr fields "12,28,c 9,B,1": "Next (F1)"
01850   pr fields "12,39,c 11,B,5": "Cancel (F5)"
01860   input fields "10,55,C 8,UET,N": t$
01870   if cmdkey=5 then goto XIT
01880   let k$=t$=uprc$(lpad$(rtrm$(t$),8))
01890   restore #1,key>=k$: eof L1900 nokey L1800
01900 L1900: pr newpage
01910   let message$=""
01920   let win =101 ! assign window #
01930   let stopable=1
01940   let fnwait(win,cap$,message$,stopable)
01950   on fkey 5 goto L2180
01960   let pg=eof4=0
01970   let fnopenprn
01980   gosub L2280
01990 L1990: read #1,using fm1$: t$,mat in1$ eof L2150
02000   pr #255: 
02010   pr #255: "EMPLOYEE #: ";t$
02020   pr #255,using L2080: "Box 11",in1$(1),val(in1$(2)),val(in1$(3)),val(in1$(4)),val(in1$(5))
02030   pr #255,using L2080: "Unused",in1$(6),val(in1$(7)),val(in1$(8)),val(in1$(9)),val(in1$(10))
02040   pr #255,using L2080: "Box 12a",in1$(11),val(in1$(12)),val(in1$(13)),val(in1$(14)),val(in1$(15))
02050   pr #255,using L2080: "Box 12b",in1$(16),val(in1$(17)),val(in1$(18)),val(in1$(19)),val(in1$(20))
02060   pr #255,using L2080: "Box 12c",in1$(21),val(in1$(22)),val(in1$(23)),val(in1$(24)),val(in1$(25))
02070   pr #255,using L2080: "Box 12d",in1$(26),val(in1$(27)),val(in1$(28)),val(in1$(29)),val(in1$(30))
02080 L2080: form pos 2,c 9,c 12,n 12.2,3*n 6,skip 1
02090   goto L1990
02100 ! ______________________________________________________________________
02110   pr #255: newpage
02120   gosub L2280
02130   continue 
02140 ! ______________________________________________________________________
02150 L2150: ! END OF PROOF LIST
02160 ! pr #255: NEWPAGE
02170   let fncloseprn
02180 L2180: on fkey 5 ignore 
02190 ! Close #255:
02200   if fnprocess=1 then goto L2210 else goto MENU1
02210 L2210: goto XIT
02220   pr #255: newpage
02230   if eof4=1 then goto L2250
02240   gosub L2280
02250 L2250: return 
02260 ! ______________________________________________________________________
02270 NEWPGE: pr #255: newpage: gosub L2280: continue 
02280 L2280: let p1=34-len(rtrm$(cnam$))/2
02290   pr #255,using L2300: date$('mm/dd/yy'),rtrm$(cnam$)
02300 L2300: form skip 2,pos 1,c 8,pos p1,c 40,skip 1
02310   let p1=34-int(len(rtrm$(dat$))/2)
02320   let pg=pg+1
02330   pr #255,using L2340: time$,"W-2 Box 12 Information Proof List",pg,dat$
02340 L2340: form pos 1,c 8,pos 13,c 50,skip 1,pos 1,"PAGE ",n 3,pos p1,c 20,skip 1
02350   return 
02360 ! ______________________________________________________________________
02370 L2370: close #1: ioerr L2380
02380 L2380: execute "Copy "&env$('Q')&"\PRmstr\W2Box16.h"&str$(cno)&",X -D -n"
02390   execute "Free "&env$('Q')&"\PRmstr\W2Box16.h"&str$(cno)&" -n"
02400   execute "Rename X "&env$('Q')&"\PRmstr\W2Box16.h"&str$(cno)&" -n"
02410   goto L1670
02420 ! ______________________________________________________________________
02430 L2430: if err=4152 then goto L1660 else goto ERTN
03140 ! ______________________________________________________________________
03150 PRNBELL: ! pr bell
03160   pr fields "24,1,C 7,N": bell$
03170   return 
03320 ! ______________________________________________________________________
03330 XNOPENWIN: ! (WIN,SR,SC,ER,EC,&CAP$)
03340   if sr<1 then let sr=10
03350   if sc<1 then let sc=20
03360   if er<1 then let er=14
03370   if ec<1 then let ec=59
03380   let win_width=ec-sc+1
03390   close #win: ioerr L3400
03400 L3400: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outin 
03410   pr #win: newpage
03420   pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": cnam$(1:min(40,win_width))
03430   pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number "&str$(cno)(1:min(40,win_width))
03440   return 
03450 ! ______________________________________________________________________
03580 ! <Updateable Region: ERTN>
03590 ERTN: let fnerror(program$,err,line,act$,"xit")
03600   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03610   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03620   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
03630 ERTN_EXEC_ACT: execute act$ : goto ERTN
03640 ! /region
03650 ! ______________________________________________________________________
