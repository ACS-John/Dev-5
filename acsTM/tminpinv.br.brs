00020 ! 
00030   on error goto L1770
00040   library 'S:\Core\Library': fnopenprn,fncloseprn,fntop,fnxit,fnsearch
00050   on fkey 5 goto L1530
00060   fntop(program$,"Preprinted Invoices")
00080   fnopenprn
00090   namtab=int(66-(len(rtrm$(env$('Program_Caption')))/2))
00100   dim scr1$(8),fl1$(11),in1$(10),ot1$(10),scrid$(3)*80,inp(9),iv$*12,a1$*30
00110   dim m$*11,fm$*54,fp$*80,pt(4),fl2$(7),scr2$(4),ot2$(4)
00120   open #1: "Name=[Q]\TMmstr\Company.h[cno],Shr",internal,input ioerr L1770
00130   read #1,using L140: dept,subac ioerr L1770
00140 L140: form pos 162,2*n 1
00150   close #1: 
00160   x8=8+subac+dept
00170   if subac=0 and dept=0 then m$="x 3,n 6,x 3"
00180   if subac=1 and dept=0 then m$="x 3,n 6,n 3"
00190   if subac=1 and dept=1 then m$="n 3,n 6,n 3"
00200   if subac=0 and dept=1 then m$="n 3,n 6,x 3"
00210   fm$="FORM POS 1,n 5,n 1,pd 4.2,n 6,n 2,"&m$&",n 2,c 12"
00220   fp$="form pos 1,n 5,n 10,n 10,n 15.2,n 8,n 8,x 7,"&m$&",n 10,x 7,c 12,skip 1"
00230   fl1$(9)="2,10,c 60,h,n"
00240   fl1$(10)="3,10,c 60,h,n"
00250   fl1$(11)="15,10,c 70,h,n"
00260   fl2$(5)="2,10,c 60,h,n"
00270   fl2$(6)="14,10,c 60,h,n"
00280   fl2$(7)="15,10,c 70,h,n"
00290   in1$(1)="5,25,n 5,ut,n"
00300   in1$(2)="6,25,n 1,ut,n"
00310   in1$(3)="7,25,n 10.2,Cu,n"
00320   in1$(4)="8,25,n 6,ut,n"
00330   in1$(5)="9,25,n 2,ut,n"
00340   if dept=1 then in1$(6)="10,20,n 3,ut,n"
00350   in1$(6+dept)="10,25,n 6,ut,n"
00360   if subac=1 then in1$(7+dept)="10,34,n 3,ut,n"
00370   in1$(7+subac+dept)="11,25,n 2,ut,n"
00380   in1$(8+subac+dept)="12,25,c 12,ut,n"
00390   for j=1 to x8
00400     if j<9 then fl1$(j)=str$(j+4)&",2,c 20"
00410     if j<5 then ot2$(j)=str$(j+4)&",25,n 10.2,ut,n"
00420     if j<5 then fl2$(j)=fl1$(j)
00430     ot1$(j)=in1$(j)
00440   next j
00450   ot1$(3)="7,25,n 10.2,ut,n"
00460   mat inp(x8-1)
00470   data "CLIENT #"
00480   data "BILLING CODE"
00490   data "AMOUNT"
00500   data "DATE"
00510   data "CATEGORY"
00520   data "G/L NUMBER"
00530   data "SUB CATEGORY"
00540   data "INVOICE #"
00550   read mat scr1$ ioerr L1770
00560   scr2$(1)="CLIENT #S"
00570   scr2$(2)="AMOUNTS"
00580   scr2$(3)="CATEGORIES"
00590   scr2$(4)="SUB CATEGORIES"
00600   open #1: "Name=[Q]\TMmstr\CLmstr.h[cno],KFName=[Q]\TMmstr\CLIndex.h[cno],Shr",internal,input,keyed ioerr L1770
00601   open #11: "Name=[Q]\TMmstr\CLmstr.h[cno],KFName=[Q]\TMmstr\CLIndx2.h[cno],Shr",internal,input,keyed ioerr L1770
00610   open #2: "Name=[Q]\TMmstr\TMWk2"&wsid$&".H[cno]",internal,outIn,relative ioerr L630
00620   close #2,free: 
00630 L630: open #2: "Name=[Q]\TMmstr\TMWk2"&wsid$&".H[cno],Replace,RecL=56",internal,outIn,relative ioerr L1770
00640 L640: scrid$(1)="TIME MANAGEMENT INPUT OF INVOICES"
00650   scrid$(2)="Enter CLIENT # as 0 when completed."
00660   scrid$(3)="PRESS F1 IF YOU HAVE ANOTHER ALLOCATION FOR THE SAME INVOICE"
00670 L670: mat inp=(0)
00680   iv=val(iv$)+1 conv L710
00690   iv$=str$(iv)
00700   goto L720
00710 L710: iv$=""
00720 L720: inp(4)=inp4
00730   inp(2)=inp2
00740 L740: pr newpage
00750   pr f mat fl1$: mat scr1$,mat scrid$
00760   pr f mat ot1$: mat inp," " ! IV$
00770   pr f "6,40,C 40,N": "(1=Partial  2=Final  3=Write-off)"
00775   pr f "23,30,c 25": "F4 Search  F5 Stop"
00780   if chg=2 then goto L870
00790 L790: if cmdkey=1 then goto L800 else input fields "5,25,N 5,UE,N": inp(1) conv L790
00795   if cmdkey=4 then goto TMSRCH
00800 L800: if inp(1)=0 or cmdkey=5 then goto L1360
00810   k$=lpad$(str$(inp(1)),5)
00820   read #1,using L970,key=k$,release: a1$ nokey L850
00830   pr f "5,45,C 30,N": a1$
00840   goto L870
00850 L850: pr f "5,45,C 18,R,N": "CLIENT NOT ON FILE"
00860   goto L790
00870 L870: input fields mat in1$: mat inp,iv$ conv L1690
00880   if cv>0 then in1$(cv)(cv1:cv2)="U"
00890   if inp(1)=0 and chg><2 then goto L1360
00900   inp4=inp(4)
00910   inp2=inp(2)
00920   if inp(1)=0 then mat inp=(0)
00930   if inp(1)=0 then iv$=""
00940   if inp(1)=0 then goto L1240
00950   k$=lpad$(str$(inp(1)),5)
00960   read #1,using L970,key=k$,release: a1$ nokey L990 ioerr L1770
00970 L970: form pos 6,c 30
00980   goto L1010
00990 L990: cv=1
01000   goto L1710
01010 L1010: if inp(2)<1 or inp(2)>3 then cv=2 else goto L1030
01020   goto L1710
01030 L1030: if inp(4)<10100 or inp(4)>123199 then cv=4 else goto L1050
01040   goto L1710
01050 L1050: if inp(5)<1 or inp(5)>10 then cv=5 else goto L1070
01060   goto L1710
01070 L1070: if env$('client')<>"ACS" then goto L1090
01072   if inp(x8-1)<0 or inp(x8-1)>24 then cv=(x8-1) else goto L1090
01080   goto L1710
01090 L1090: pt(1)=pt(1)+inp(1)
01100   pt(2)=pt(2)+inp(3)
01110   pt(3)=pt(3)+inp(5)
01120   pt(4)=pt(4)+inp(x8-1)
01130   if chg=2 then goto L1240
01140   rw=rw+1
01150   write #2,using fm$,rec=rw: mat inp,iv$
01160   if cmdkey=1 then goto L1170 else goto L670
01170 L1170: inp(3)=0
01180   inp(5)=0
01190   inp(6)=0
01200   inp(6+subac)=0
01210   inp(6+subac+dept)=0
01220   inp(7+subac+dept)=0
01230   goto L740
01240 L1240: rewrite #2,using fm$,rec=rr: mat inp,iv$
01250 L1250: pr newpage
01260   pr f "10,10,c 60": "ENTER REF # TO CORRECT; ENTER 0 WHEN COMPLETED"
01270 L1270: input fields "10,60,n 5,eu,n": rr conv L1270
01280   if rr=0 then goto L1360
01290   if rr>rw or rr<1 then goto L1270
01300   read #2,using fm$,rec=rr: mat inp,iv$ ioerr L1770
01310   pt(1)=pt(1)-inp(1)
01320   pt(2)=pt(2)-inp(3)
01330   pt(3)=pt(3)-inp(5)
01340   pt(4)=pt(4)-inp(x8-1)
01350   goto L740
01360 L1360: pr newpage
01370   scrid$(1)="TIME MANAGEMENT INPUT PROOF TOTALS"
01380   scrid$(2)="Enter 1 for a listing, 2 for corrections,"
01390   scrid$(3)=" 3 for additional entries, 4 to merge, 5 stop without posting."
01400   pr f mat fl2$: mat scr2$,mat scrid$
01410   pr f mat ot2$: mat pt
01420 L1420: input fields "16,30,n 1,eu,n": chg conv L1420
01430   on chg goto L1440,L1620,L640,L1660,L1680 none L1420
01440 L1440: pr newpage
01450   pr f "10,10,c 60,h,n": "TIME MANAGEMENT CORRECTION LISTING IN PROCESS"
01460   pr f "23,2,C 50,N": "Press F5 to stop!"
01470   gosub L1550
01480   for j=1 to rw
01490     read #2,using fm$,rec=j: mat inp,iv$ ioerr L1770
01500     if inp(1)=0 then goto L1520
01510     pr #255,using fp$: j,mat inp,iv$ pageoflow L1590
01520 L1520: next j
01530 L1530: fncloseprn
01540   goto L1360
01550 L1550: pr #255,using L1560: date$,env$('Program_Caption'),time$
01560 L1560: form skip 2,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,skip 2
01570   pr #255: "REF #  CLIENT #  BILLING-CODE     AMOUNT   DATE   CATEGORIES   G/L---NUMBER  SUB-CATEGORY   INVOICE #"
01580   return 
01590 L1590: pr #255: newpage
01600   gosub L1550
01610   continue 
01620 L1620: scrid$(1)="TIME MANAGEMENT INPUT CORRECTION SCREEN"
01630   scrid$(2)="ENTER CLIENT # AS 0 TO DELETE THIS ENTRY"
01640   scrid$(3)=""
01650   goto L1250
01660 L1660: close #1: 
01670   close #2: 
01680 L1680: chain "S:\acsTM\TMMRGMAN"
01685 XIT: fnxit
01690 L1690: if cv>0 then in1$(cv)(cv1:cv2)="U"
01700   cv=cnt+1
01710 L1710: pr f "24,78,C 1": bell
01720   in1$(cv)=rtrm$(in1$(cv))
01730   cv1=pos(uprc$(in1$(cv)),"U",1)
01740   cv2=cv1+1
01750   in1$(cv)(cv1:cv1)="CR"
01760   goto L870
01770 L1770: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L1790
01780   goto L1830
01790 L1790: pr newpage
01800   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L1820
01810   goto L1830
01820 L1820: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
01830 L1830: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
01840   input fields "24,60,C 1,N": quitcode$
01850   if err=61 and rtrm$(uprc$(quitcode$))="Q" then goto L1360 else goto L1890
01860   pr f "23,3,C 78,N": ""
01870   pr f "24,3,C 78,N": ""
01880   retry 
01890 L1890: goto XIT
04800 TMSRCH: ! search for customer #
04810   dim heading$*70,form$*80,numeric_format$*20,selection$*70
04820   file_num=11 ! alpha index on clients
04830   form$="form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2"
04840   numeric_format$='pic($$$,$$$.##)'
04850   key_length=5
04860   heading$="Acct #횼ame컴컴컴컴컴컴컴컴컴컴Address컴컴컴컴Balance"
04870   fnsearch(cap$,file_num,heading$,form$,numeric_format$,selection$,key_length)
04880   k$=z$=selection$ ! pull key from first field in search line
04890   inp(1)=0
04900   inp(1)=val(selection$) conv L4910
04910 L4910: goto L740
