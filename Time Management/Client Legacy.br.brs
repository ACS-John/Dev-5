00010 ! formerly S:\acsTM\ClMaint
00020   on error goto L4170
00030 ! 
00040   library "S:\Core\search.br": fnsearch
00050   library "S:\Core\Library.br": fnerror,fnmsgbox,fnwin3b
00060   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fnpedat$,fnprocess, fnTos,fnLbl,fnTxt,fnChk,fnqgl,fnCmdSet,fnAcs,fnagl$
00070   fntop(program$)
00100 ! 
00110 ! 
00120   dim ar(5),ph2$*12,ss2$*11,arta(2),des$*30,cm$*70
00130   dim z$*5,a$(5)*30,ph$*12,ss$*11,dd(10),sc(10),ca(10),ta(25,2),fb(25)
00140   dim z$(3)*5,ph$(6)*12,ss$(6)*11,h$(3,6)*30,l(3,10),m(3,10),pno(3),mye(3),har(3,5)
00150   dim fl1$(33),oi1$(38),scr1$(31)*20,scrid$(2)*40
00160   dim k$*5,d$*9,b(8),sc$*4,iv$*12,fl2$(15),scr2$(14)*25,ot2$(14),in2$(14)
00170   dim flit$(6)*18,scrt$(6)*20,scrz$(2)*79,desc$(8)*18
00180   dim hlp$(20)*78,flh$(22)*18,hhd$*60
00190   dim p$*5,iv$*12,tr(6),id$*20 ,cat$(10)*30
00200   dim st$(20)*24,scot$(21),sct$(40),app(20),ma(20)
00210   dim st2$(20)*24,ap2(20),ma2(20)
00220   dim wrd1$(10)*33
00230   open #1: "Name=S:\acsTM\TMSCRN.CL,Shr",internal,input ioerr L4170
00240   read #1,using L250: mat scr1$,mat fl1$,mat oi1$,mat scr2$,mat fl2$,mat ot2$,mat in2$,mat flh$,mat flit$,mat desc$,mat scrt$ ioerr L4170
00250 L250: form pos 1,31*c 20,71*c 18,14*c 25,79*c 18,6*c 20
00260   close #1: 
00270   scrt$(5)=scrt$(6)=""
00280   for j=22 to 31: scr1$(j)=rtrm$(scr1$(j)): next j
00290   gosub L4300
00300   open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],Shr,KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,outIn,keyed ioerr L4160
00310   open #11: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],Shr,KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr",internal,outIn,keyed ioerr L4160
00320   open #2: "Name=S:\Core\Data\acsllc\TMTrans.h[cno],Shr",internal,outIn,relative 
00330   open #3: "Name=S:\Core\Data\acsllc\TMTRAddr.h[cno],Shr",internal,input,relative 
00340   open #4: "Name=S:\Core\Data\acsllc\ARTrans.h[cno],Shr",internal,outIn,relative 
00350   open #5: "Name=S:\acsTM\CLMst.Hlp,Shr",internal,outIn,relative ioerr L4170
00360   open #6: "Name=S:\Core\Data\acsllc\TMCat.h[cno],Shr",internal,outIn,relative ioerr L380
00370   goto L390
00380 L380: chain "S:\acsTM\CTMAINT"
00390 L390: read #6,using L400,rec=1: mat cat$ ioerr L4170
00400 L400: form pos 1,10*c 30
00410   close #6: 
00420 L420: pr newpage
00430   pr f "3,10,Cc 60,R,N": "Client File"
00440   pr f "4,10,Cc 60,R,N": "Company [cno] - "&ltrm$(env$('cnam'))
00450   wrd1$(1)="1. Initial File Preparation" !:
        wrd1$(2)="2. Add " !:
        wrd1$(3)="3. Edit or View" !:
        wrd1$(4)="4. pr Proof List" !:
        wrd1$(5)="5. Reassign Transaction Addresses" !:
        wrd1$(6)="6. pr Directory" !:
        wrd1$(7)="7.       Listing" !:
        wrd1$(8)="8.       Labels" !:
        wrd1$(9)="9. Reset Status Codes" !:
        wrd1$(10)="" ! was 10. search by name
00460   for j=1 to 10 !:
          io1$(j)=str$(j+5)&",9,C 33,N" !:
        next j !:
        io1$(3)="8,9,C 33,C,N"
00470   pr f "17,9,C 60,B,99": "Exit (Esc or  F5)"
00480 L480: rinput select mat io1$,attr "H": mat wrd1$
00490   ti=curfld
00500   if cmdkey=5 or cmdkey=99 then let fnxit
00510 L510: on ti goto L420,L580,L630,L2130,L2740,L530,L540,LABELS,L560,L570 none L480
00520 L520: chain "S:\acsTM\TMBLDCL"
00530 L530: if new1=1 then goto L1970 else chain "S:\acsTM\TMCLIDIR"
00540 L540: if new1=1 then goto L1970 else chain "S:\acsTM\TMCLILST"
00550 LABELS: if new1=1 then goto L1970 else chain "S:\acsTM\CLILabel.wb"
00560 L560: if new1=1 then goto L1970 else chain "S:\acsTM\CLRSETST"
00570 L570: if new1=1 then goto L1970 else chain "S:\acsTM\TMNAMINQ"
00580 L580: scrid$(1)="  TIME MANAGEMENT ADD CLIENTS"
00590   scrid$(2)="  (ENTER CLIENT # AS 0 WHEN COMPLETED)"
00600   mat ca=(0)
00610   mat arta=(0)
00620   goto L760
00630 L630: scrid$(1)="  TIME MANAGEMENT MAINTAIN CLIENTS"
00640   scrid$(2)="  (ENTER CLIENT # AS 0 TO DELETE)"
00650 L650: pr newpage
00660   pr f "10,30,C 14,N": "Client Number:"
00670   pr f "22,28,c 40": "Cancel F5    Search  F6"
00680 L680: input fields "10,45,Nz 5,UT,N": ano conv L680
00685   if cmdkey=5 then goto L420
00690   if cmdkey=6 then goto TMSRCH
00700 L700: if ano=0 or cmdkey=5 or cmdkey=99 then goto L420
00710   z$=lpad$(str$(ano),5)
00720   read #1,using L730,key=z$: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2 nokey L650 ioerr L4170
00730 L730: form pos 1,c 5,5*c 30,c 12,c 11,n 9,n 2,10*pd 3,10*n 1,10*pd 3,c 12,c 11,2*pd 5.2,pd 4.3,2*n 1,2*pd 3,c 70,20*n 1,20*pd 3.2,20*n 1,20*pd 3.2
00740   hbal=ar(1): chgbal=0
00750   d$=z$
00760 L760: hsr=1: pr newpage
00770   pr f mat fl1$: mat scr1$,mat scrid$
00780   if ti=3 then pr f mat oi1$: val(z$),mat a$,ph$,ss$,pno,mye,ph2$,ss2$,mat ar,cm$,mat dd,mat sc
00790   pr f "24,30,C 40,N": "F1 Save  F5 Cancel F6=HELP"
00800 L800: input fields mat oi1$: x1,mat a$,ph$,ss$,pno,mye,ph2$,ss2$,mat ar,cm$,mat dd,mat sc conv L2980
00810   if cmdkey=5 then goto L650
00820   if cv>0 then oi1$(cv)(cv1:cv2)="U": cv=0
00830   if cmdkey=6 then goto L3880
00840   if ti=2 and x1=0 then goto L420
00850   z$=lpad$(str$(x1),5)
00860   if x1=0 then goto L1100
00870   if mye<1 or mye>12 then cv=10: goto L3000
00880   if ar(5)<1 or ar(5)>2 then cv=17: goto L3000
00890   for j=1 to 10
00900     if dd(j)=0 then goto L930
00910     if dd(j)>0 and dd(j)<5 then goto L930
00920     if dd(j)<101 or dd(j)>1231 then cv=18+j: goto L3000
00930 L930: if sc(j)<0 or sc(j)>2 then cv=28+j: goto L3000
00940   next j
00950   if ti=3 and hbal<>ar(1) then chgbal=1 else chgbal=0
00960   if ti=2 then goto L980
00970   if x1=ano then goto L1020
00980 L980: read #1,using L990,key=z$: z$ nokey L1020 ioerr L4170
00990 L990: form pos 1,c 5
01000   cv=1: goto L3000
01010   goto L800
01020 L1020: pr newpage
01030 ! if cno<> 500 then goto 1080
01040   pr f mat scot$: mat st$,"ACCOUNT "&z$&"     APPLICATIONS  MAINTENANCE"
01050   if ti=3 then pr f mat sct$: mat app,mat ma
01060 L1060: input fields mat sct$,attr "r": mat app,mat ma conv L1060
01070   gosub CAT2
01080   if ti=2 then goto L1210
01090   if x1=ano then goto L1620
01100 L1100: pr newpage
01110   pr f "10,10,c 43,n": "ACCOUNT # "&d$&" WILL BE DELETED"
01120   pr f "11,10,c 43,n": "ENTER 1 TO DELETE, ENTER 2 TO RE-ENTER"
01130 L1130: input fields "11,60,n 1,eu,n": delact conv L1130
01140   if delact=2 then goto L510
01150   if delact><1 then goto L1100
01160   delete #1,key=d$: 
01170   z$=""
01180   gosub L1390
01190   new1=1
01200   goto L510
01210 L1210: if ti=2 then goto L1230 else delete #1,key=d$: nokey L1230
01220   gosub L1390
01230 L1230: write #1,using L730: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2
01240   new1=1
01250   if ti<>2 or ar(1)=0 then goto L1380
01260   if ar(1)<0 then tr(5)=4 else tr(5)=1
01270   tr(1)=val(date$(4:5)&date$(7:8)&date$(1:2))
01280   tr(2)=abs(ar(1))
01290   tr(3)=tr(2)
01300   id$="BEGINNING BALANCE"
01310   nta=0
01320   rec4=lrec(4)+1
01330   write #4,using L3420,rec=rec4: z$,iv$,mat tr,id$,nta
01340   rewrite #4,using L1350,rec=1: rec4
01350 L1350: form pos 58,pd 3
01360   mat arta=(rec4)
01370   rewrite #1,using L2930,key=z$: mat arta
01380 L1380: goto L510
01390 L1390: ! CHANGE TRANS
01400   if x1=ano then goto L1610
01410   for j=1 to 10
01420     if ca(j)=0 then goto L1540
01430     read #3,using L1750,rec=ca(j): mat ta noRec L1540
01440     for j1=1 to 25
01450       rec2=ta(j1,1)
01460 L1460: if rec2=0 then goto L1530
01470       read #2,using L1480,rec=rec2: nta noRec L1530
01480 L1480: form pos 54,pd 3
01490       rewrite #2,using L1500,rec=rec2: z$
01500 L1500: form pos 1,c 5
01510       rec2=nta
01520       goto L1460
01530 L1530: next j1
01540 L1540: next j
01550   rec4=arta(1)
01560 L1560: if rec4=0 then goto L1610
01570   read #4,using L1350,rec=rec4: nta
01580   rewrite #4,using L1500,rec=rec4: z$
01590   rec4=nta
01600   goto L1560
01610 L1610: return 
01620 L1620: rewrite #1,using L730,key=z$: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2
01630   if sum(ca)=0 then goto L3320
01640   pr newpage
01650   pr f "10,10,c 60,n": "ENTER 1 TO REVIEW CATEGORY TRANSACTIONS; ELSE ENTER 0"
01660 L1660: input fields "10,65,N 1,UE,N": det conv L1660
01670   on det+1 goto L3320,L1680 none L1660
01680 L1680: pr newpage
01690   pr f "10,10,c 52": "Enter Category; Enter 0 when completed"
01700 L1700: input fields "10,55,N 2,UE,N": det conv L1700
01710   if det=0 then goto L3320
01720   if det<1 or det>10 then goto L1700
01730   if ca(det)=0 then goto L1680
01740   read #3,using L1750,rec=ca(det): mat ta,mat fb noRec L1680 ioerr L4170
01750 L1750: form pos 1,50*pd 3,25*n 1
01760 L1760: pr newpage
01770   pr f "10,10,c 52": "Enter Sub-Category; Enter 99 when completed"
01780 L1780: input fields "10,55,N 2,UE,N": det conv L1780
01790   if det=99 then goto L1680
01800   if det=0 then det=25
01810   if det<1 or det>25 then goto L1780
01820   if ta(det,1)=0 then goto L1760
01830   tadr=ta(det,1)
01840 L1840: read #2,using L1850,rec=tadr: k$,e$,mat b,sc$,iv$,nta,des$ ioerr L4170
01850 L1850: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30
01860   pr newpage
01870   if fb(det)=1 then pr f "2,27,c 20": "PARTIAL BILLED"
01880   if fb(det)=2 then pr f "2,27,c 20": "FINAL BILLED"
01890   pr f mat fl2$: mat scr2$,a$(1)
01900   pr f mat ot2$: k$,e$,mat b,sc$,iv$,nta,des$
01910 L1910: input fields mat ot2$: k$,e$,mat b,sc$,iv$,qta,des$ conv L3140
01920   iv$=lpad$(rtrm$(iv$),12)
01930   rewrite #2,using L1850,rec=tadr: k$,e$,mat b,sc$,iv$,nta,des$
01940   if nta=0 then goto L1760
01950   tadr=nta
01960   goto L1840
01970 L1970: close #1: 
01980   close #2: 
01990   close #3: 
02000   close #4: 
02010   close #5: 
02020   close #11: 
02030   if cmdkey=5 then let fnxit
02040   execute "Index S:\Core\Data\acsllc\CLmstr.h[cno]"&' '&"S:\Core\Data\acsllc\CLIndex.h[cno] 1 5 REPLACE DupKeys -n"
02050   execute "Index S:\Core\Data\acsllc\CLmstr.h[cno]"&' '&"S:\Core\Data\acsllc\CLIndx2.h[cno] 6 28 REPLACE DupKeys -n"
02060   if ti=6 then chain "S:\acsTM\TMCLIDIR"
02070   if ti=7 then chain "S:\acsTM\TMCLILST"
02080   if ti=8 then chain "S:\acsTM\CLILABEL"
02090   if ti=9 then chain "S:\acsTM\CLRSETST"
02100   if ti=10 then chain "S:\acsTM\TMNAMINQ"
02110 PROOF_LIST: ! 
02120   fnopenprn
02130 L2130: restore #1,key>="     ": 
02140   on fkey 5 goto L2330
02150   pr newpage !:
        pr f "10,15,C 50,N": "CLIENT PROOF LISTING IN PROCESS" !:
        pr f "12,30,Cc 20,B,5": "Cancel (F5)"
02160   namtab=66-int(len(rtrm$(env$('cnam')))/2)
02170   r=1
02180 L2180: read #1,using L730: z$(r),mat a$,ph$(r),ss$(r),pno(r),mye(r),mat dd,mat sc,mat ca,ph$(r+3),ss$(r+3),mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2 eof L2300 ioerr L4170
02190   for j=1 to 10
02200     if j>5 then goto L2230
02210     har(r,j)=ar(j)
02220     h$(r,j)=a$(j)
02230 L2230: l(r,j)=dd(j)
02240     m(r,j)=sc(j)
02250   next j
02260   r=r+1
02270   if r<4 then goto L2180
02280   gosub L2350
02290   goto L2180
02300 L2300: if r>1 then gosub L2350
02310   fncloseprn
02320   goto L420
02330 L2330: pr #255: newpage
02340   goto L420
02350 L2350: gosub L2680
02360   pr #255,using L2370: "CLIENT NUMBER",z$(1),z$(2),z$(3)
02370 L2370: form pos 1,c 25,3*c 35,skip 1
02380   for j=1 to 5
02390     pr #255,using L2370: rtrm$(scr1$(j+1)),h$(1,j),h$(2,j),h$(3,j)
02400   next j
02410   pr #255,using L2370: "BUSINESS PHONE #",ph$(1),ph$(2),ph$(3)
02420   pr #255,using L2370: "FEDERAL ID # OR SS #",ss$(1),ss$(2),ss$(3)
02430   pr #255,using L2450: "PARTNER NUMBER",pno(1),pno(2),pno(3)
02440   pr #255,using L2450: "MONTH OF YEAR-END",mye(1),mye(2),mye(3)
02450 L2450: form pos 1,c 25,n 12,pos 64,n 12,pos 102,n 12,skip 1
02460   pr #255,using L2370: "FAX PHONE #",ph$(4),ph$(5),ph$(6)
02470   pr #255,using L2370: "SPOUCE SS #",ss$(4),ss$(5),ss$(6)
02480   for j=1 to 5
02490     pr #255,using L2500: scr1$(j+12),har(1,j),har(2,j),har(3,j)
02500 L2500: form pos 1,c 25,n 12.2,pos 64,n 12.2,pos 102,n 12.2,skip 1
02510   next j
02520   for j=1 to 10
02530     pr #255,using L2450: "DUE DATE-"&cat$(j)(1:15),l(1,j),l(2,j),l(3,j)
02540     pr #255,using L2450: "STATUS CODE-"&cat$(j)(1:12),m(1,j),m(2,j),m(3,j)
02550   next j
02560   pr #255: newpage
02570   mat z$=(" ")
02580   mat h$=(" ")
02590   mat ph$=(" ")
02600   mat ss$=(" ")
02610   mat pno=(0)
02620   mat mye=(0)
02630   mat l=(0)
02640   mat m=(0)
02650   r=1
02660   mat har=(0)
02670   return 
02680 L2680: p1=p1+1
02690   pr #255,using L2700: date$,env$('cnam'),"PAGE",p1
02700 L2700: form skip 3,pos 1,c 8,pos namtab,c 40,pos 100,c 5,n 4,skip 1
02710   pr #255,using L2720: time$,"TIME MANAGEMENT CLIENT FILE PROOF LIST"
02720 L2720: form pos 1,c 8,pos 48,c 38,skip 2
02730   return 
02740 L2740: pr newpage
02750   pr f "10,15,c 60,h,n": "REASSIGN TRANSACTION ADDRESSES IN PROCESS"
02760   restore #1,key>="     ": 
02770 L2770: read #1,using L2780: mat ca,mat arta eof L2830 ioerr L4170
02780 L2780: form pos 230,10*pd 3,pos 299,2*pd 3
02790   mat ca=(0)
02800   mat arta=(0)
02810   rewrite #1,using L2780: mat ca,mat arta
02820   goto L2770
02830 L2830: lr4=lrec(4)
02840   rewrite #4,using L2950,rec=1: lr4
02850   for j=1 to lr4
02860     read #4,using L2870,rec=j: z$,nta noRec L2960 ioerr L4170
02870 L2870: form pos 1,c 5,pos 58,pd 3
02880     read #1,using L2930,key=z$: mat arta nokey L2960 ioerr L4170
02890     if arta(1)=0 then arta(1)=j
02900     if arta(2)>0 then rewrite #4,using L2950,rec=arta(2): j
02910     arta(2)=j
02920     rewrite #1,using L2930: mat arta
02930 L2930: form pos 299,2*pd 3
02940     rewrite #4,using L2950,rec=j: 0
02950 L2950: form pos 58,pd 3
02960 L2960: next j
02970   chain "S:\acsTM\TMFIXADR"
02980 L2980: if cv>0 then oi1$(cv)(cv1:cv2)="U"
02990   cv=cnt+1
03000 L3000: pr f "24,78,C 1": bell
03010 L3010: oi1$(cv)=rtrm$(oi1$(cv))
03020   cv1=pos(uprc$(oi1$(cv)),"U",1)
03030   cv2=cv1+1
03040   oi1$(cv)(cv1:cv1)="CR"
03050   if chgbal=1 then goto L760 else goto L800
03060 L3060: if cv>0 then flit$(cv)(cv1:cv2)="U"
03070   cv=cnt+1
03080 L3080: pr f "24,78,C 1": bell
03090   flit$(cv)=rtrm$(flit$(cv))
03100   cv1=pos(uprc$(flit$(cv)),"U",1)
03110   cv2=cv1+1
03120   flit$(cv)(cv1:cv1)="RC"
03130   goto L3630
03140 L3140: if cv>0 then ot2$(cv)(cv1:cv2)="U"
03150   cv=cnt+1
03160   pr f "24,78,C 1": bell
03170   ot2$(cv)=rtrm$(ot2$(cv))
03180   cv1=pos(uprc$(ot2$(cv)),"U",1)
03190   cv2=cv1+1
03200   ot2$(cv)(cv1:cv1)="RC"
03210   goto L1910
03220 L3220: pr newpage
03230   pr f "8,10,C 60,H,N": "CURRENT BALANCE DOES NOT AGREE WITH TOTAL TRANSACTIONS"
03240   pr f "10,21,C 78": "CURRENT BALANCE = "
03250   pr f "10,40,N 12.2,U,N": ar(1)
03260   pr f "11,18,C 60": "TOTAL TRANSACTIONS = "
03270   pr f "11,40,N 12.2,U,N": tt
03280   pr f "13,5,C 70": "ENTER 1 TO CORRECT CURRENT BALANCE; ENTER 2 TO CORRECT TRANSACTIONS"
03290 L3290: input fields "15,37,n 1,eu,n": chgbal conv L3290
03300   if chgbal=1 then cv=13: goto L3010
03310   if chgbal=2 then goto L3380 else goto L3290 ! 9/21/87
03320 L3320: if chgbal>0 then goto L3380
03330   if arta(1)=0 then goto L510
03340   pr newpage
03350   pr f "10,5,C 61": "ENTER 1 TO REVIEW A/R TRANSACTIONS OR 0 TO CONTINUE:"
03360 L3360: input fields "10,60,N 1,UE,N": det conv L3360
03370   on det+1 goto L510,L3380 none L3360
03380 L3380: tt=0
03390   rec4=arta(1)
03400   if arta(1)=0 then goto L3760
03410 L3410: read #4,using L3420,rec=rec4: z$,iv$,mat tr,id$,nta ioerr L4170
03420 L3420: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
03430   scrz$(1)="ACCOUNT #"&z$&"   BALANCE ="&str$(ar(1))&"   TOTAL TRANSACTIONS ="&str$(tt)
03440   scrz$(2)=" "
03450   tp$=""
03460   on tr(5) goto L3470,L3490,L3510,L3530,L3550,L3570 none L3580
03470 L3470: tp$="INVOICE"
03480   goto L3580
03490 L3490: tp$="FINANCE CHARGE"
03500   goto L3580
03510 L3510: tp$="STANDARD CHARGE"
03520   goto L3580
03530 L3530: tp$="COLLECTION"
03540   goto L3580
03550 L3550: tp$="DEBIT MEMO"
03560   goto L3580
03570 L3570: tp$="CREDIT MEMO"
03580 L3580: hsr=2: pr newpage
03590   pr f mat desc$: mat scrz$,mat scrt$
03600   pr f "24,2,C 78,N": "F6=HELP"
03610   pr f mat flit$: iv$,tr(1),tr(3),id$
03620   pr f "6,40,C 15,H,N": tp$
03630 L3630: input fields mat flit$,attr "R": iv$,tr(1),tr(3),id$ conv L3060
03640   if cv>0 then flit$(cv)(cv1:cv2)="U": cv=0
03650   if cmdkey=6 then goto L3880
03660   if tr(1)<10100 or tr(1)>123199 then cv=2: goto L3080
03670   if tr(5)=4 or tr(5)=6 then goto L3700
03680   tt=tt+tr(3)
03690   goto L3710
03700 L3700: tt=tt-tr(3)
03710 L3710: iv$=lpad$(rtrm$(iv$),12)
03720   rewrite #4,using L3420,rec=rec4: z$,iv$,mat tr,id$,nta
03730   if nta=0 then goto L3760
03740   rec4=nta
03750   goto L3410
03760 L3760: if tt><ar(1) then goto L3220
03770   goto L510
03780   rec4=arta(1)
03790   if arta(1)=0 then goto L3870
03800 L3800: read #4,using L3820,rec=rec4: amt,nta ioerr L4170
03810   rewrite #4,using L3830,rec=rec4: "",0,nta
03820 L3820: form pos 29,pd 5.2,pos 58,pd 3
03830 L3830: form pos 1,c 5,pos 29,pd 5.2,pos 58,pd 3
03840   if nta=0 then goto L3870
03850   rec4=nta
03860   goto L3800
03870 L3870: return 
03880 L3880: pr newpage
03890   cv=currow
03900   col=curcol
03910   if cv<1 or cv>24 then cv=0: goto L4110
03920   on hsr goto L3930,L4010
03930 L3930: cv=cv-1
03940   oi1$(cv)=rtrm$(oi1$(cv))
03950   cv1=pos(uprc$(oi1$(cv)),"U",1)
03960   cv2=cv1+1
03970   oi1$(cv)(cv1:cv1)="UC"
03980   hhd$=scr1$(cv)
03990   hr=cv
04000   goto L4080
04010 L4010: cv=cv/2-2
04020   flit$(cv)=rtrm$(flit$(cv))
04030   cv1=pos(uprc$(flit$(cv)),"U",1)
04040   cv2=cv1+1
04050   flit$(cv)(cv1:cv1)="UC"
04060   hhd$=scrt$(cv)
04070   hr=cv+24
04080 L4080: read #5,using L4090,rec=hr: mat hlp$ noRec L4150 ioerr L4170
04090 L4090: form pos 1,20*c 78
04100   pr f mat flh$: mat hlp$,hhd$,"ENTER 0 TO CONTINUE OR 1 TO UPDATE HELP SCREEN:"
04110 L4110: input fields "24,69,N 1,EU,N": j2 conv L4110
04120   if j2<>1 then goto L4150
04130   input fields mat flh$: mat hlp$
04140   rewrite #5,using L4090,rec=hr: mat hlp$
04150 L4150: on hsr goto L760,L3580
04160 L4160: if err=4152 then goto L520
04170 L4170: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L4190
04180   goto L4230
04190 L4190: pr newpage
04200   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L4220
04210   goto L4230
04220 L4220: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
04230 L4230: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
04240   input fields "24,60,C 1,N": quitcode$
04250   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
04260   pr f "23,3,C 78,N": ""
04270   pr f "24,3,C 78,N": ""
04280   retry 
04290 XIT: fnxit
04300 L4300: data " 1. GENERAL LEDGER"
04310   data " 2. ACCOUNTS RECEIVABLE"
04320   data " 3. ACCOUNTS PAYABLE"
04330   data " 4. Utility Billing"
04340   data " 5. PATIENT BILLING"
04350   data " 6. PROPERTY TAX"
04360   data " 7. HOSPITAL A/R"
04370   data " 8. FIXED ASSET"
04380   data " 9. TIME MANAGEMENT"
04390   data "10. CASH REGISTER"
04400   data "11. POINT OF SALE"
04410   data "12. INVOICING"
04420   data "13. INVENTORY"
04430   data "14. PAYROLL"
04440   data "15. PURCHASE ORDER"
04450   data "16. MUNICIPAL COURT"
04460   data "17. PCAnywhere"
04470   data "18. Checkbook"
04480   data "19. HARDWARE"
04490   data "20. OTHER"
04500   read mat st$
04510   for j=1 to 20
04520     scot$(j)=str$(j+2)&",2,C 24"
04530     sct$(j)=str$(j+2)&",27,N 1,Ut,N"
04540     sct$(j+20)=str$(j+2)&",35,N 6.2,Ut,N"
04550   next j
04560   scot$(21)="1,2,C 60,H,N"
04570   data "21. JOB COST"
04580   data "22. BUSINESS LICENSE"
04590   data "23. BUDGET MANAGEMENT"
04600   data "24. GAS AND DIESEL"
04610   data "25. ENERGY ASSISTANCE"
04620   data "26."
04630   data "27."
04640   data "28."
04650   data "29."
04660   data "30."
04670   data "31."
04680   data "32."
04690   data "33."
04700   data "34."
04710   data "35."
04720   data "36."
04730   data "37."
04740   data "38."
04750   data "39."
04760   data "40."
04770   read mat st2$
04780   return 
04790 CAT2: pr newpage
04800   pr f mat scot$: mat st2$,"ACCOUNT "&z$&"     APPLICATIONS  MAINTENANCE"
04810   if ti=3 then pr f mat sct$: mat ap2,mat ma2
04820 L4820: input fields mat sct$,attr "R": mat ap2,mat ma2 conv L4820
04830   return 
04840 TMSRCH: ! search for customer #
04850   dim heading$*70,form$*80,numeric_format$*20,selection$*70
04860   file_num=11 ! alpha index on clients
04870   form$="form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2"
04880   numeric_format$='pic($$$,$$$.##)'
04890   key_length=5
04900   heading$="Acct #횼ame컴컴컴컴컴컴컴컴컴컴Address컴컴컴컴Balance"
04910   fnsearch(env$('progrgam_caption'),file_num,heading$,form$,numeric_format$,selection$,key_length)
04920   k$=z$=selection$ ! pull key from first field in search line
04930   ano=0
04940   ano=val(selection$) conv L4950
04950 L4950: goto L700
