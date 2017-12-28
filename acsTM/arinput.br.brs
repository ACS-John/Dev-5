00020   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fnTos,fnLbl,fnTxt,fnChk,fnqgl,fnCmdSet,fnAcs,fnagl$,fnsearch
00030   fntop(program$,cap$="Collections")
00040   fncno(cno,cnam$)
00060   dim fl1$(7),flo1$(11),sc3$(5),pt(6),f3$*255,flo3$(6),name$*25
00070   dim p$*5,iv$*12,tr(6),id$*20,sc1$(5),sc2$(9),hd$(2)*50,cnam$*40
00080   dim flo4$(5),sc4$(5),ot4$(5),fli4$(5),q(3),gln1(3),gln2(3),otgl$(3)
00090   dim gl(10,4),fli1$(49),ot1$(49),pgl(3)
00100   open #h_company:=1: "Name="&env$('Q')&"\TMmstr\Company.h"&env$('cno')&",Shr",internal,input ioerr L2290
00110   read #h_company,using L130: i3,i4,i5,mat gln1,mat gln2 ioerr L2290
00120 ! i3=1 ! ENTER G/L #'S
00130 L130: form pos 161,3*n 1,pos 178,n 3,n 6,n 3,n 3,n 6,n 3
00140   close #h_company: 
00150   namtab=66-int(len(rtrm$(cnam$))/2)
00160   otgl$(1)="9,30,pic(zzz)"
00170   otgl$(2)="9,34,pic(zzzzzz)"
00180   otgl$(3)="9,41,pic(zzz)"
00190   if i3=0 then goto L490
00200   if i4=1 and i5=1 then goto L300
00210   if i4=0 and i5=1 then goto L350
00220   if i4=1 and i5=0 then goto L420
00230 ! NO DEPT    NO SUBACCOUNT
00240   sz=5
00250   gx=2
00260   mat gl(10,2)=(0)
00270   mat pgl(1)=(0)
00280   gpx=1
00290   goto L510
00300 L300: ! YES DEPT   YES SUBACCOUNT
00310   sz=2
00320   gx=4
00330   gpx=2
00340   goto L510
00350 L350: ! NO DEPT    YES SUBACCOUNT
00360   sz=3
00370   gx=3
00380   mat gl(10,3)=(0)
00390   mat pgl(2)=(0)
00400   gpx=1
00410   goto L510
00420 L420: ! YES DEPT    NO SUB ACCOUNT
00430   sz=4
00440   gx=3
00450   mat gl(10,3)=(0)
00460   mat pgl(2)=(0)
00470   gpx=2
00480   goto L510
00490 L490: ! NO GL TO BE ENTERED
00500   sz=6
00510 L510: open #h_addr:=3: "Name="&env$('Temp')&"\Addr."&session$,internal,outIn ioerr L530
00520   close #h_addr,free: 
00530 L530: open #h_addr:=3: "Name="&env$('Temp')&"\Addr."&session$&",SIZE=0,RecL=239",internal,outIn,relative ioerr L2290
00540   open #1: "Name=S:\acsTM\TMSCRN.CL,Shr",internal,input,relative ioerr L2290
00550   read #1,using L560,rec=sz: f3$,mat fl1$,mat sc1$,mat sc2$,mat fli1$,mat ot1$,mat flo1$,mat flo3$,mat sc3$ ioerr L2290
00560 L560: form pos 1,c 255,142*c 18
00570   close #1: 
00580   open #9: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&env$('cno')&",Shr",internal,input,keyed ioerr L2290
00585   open #11: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\TMmstr\CLIndx2.h"&env$('cno')&",Shr",internal,input,keyed ioerr L2290
00590 L590: hd$(1)="A/R Input Selection Menu"
00600   hd$(2)="ENTER SELECTION"
00610 L610: pr newpage
00620   pr f mat fl1$: mat sc1$,mat hd$
00630 L630: input fields "13,29,n 1,eu,n": tr5 conv L630
00640   if tr5=0 then goto L1690
00650   if tr5<1 or tr5>4 then goto L630
00660   hd$(1)="A/R INPUT "&sc1$(tr5+1)(5:18)
00670   hd$(2)="Client Number as 0 to stop"
00680 L680: if tr5=4 or tr5=3 then sc2$(7)="G/L # TO CREDIT" else sc2$(7)="G/L # TO Debit"
00690   if tr5=3 then sc2$(6)="DISCOUNT AMOUNT" else sc2$(6)=""
00700   if gx=0 then sc2$(7)=" "
00710 L710: pr newpage
00720   pr f mat flo1$: mat sc2$,mat hd$
00730   ps1=0
00740   if vf=0 then goto L790
00750   if gx><0 then goto L780
00760 L760: pr f mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2)
00770   goto L790
00780 L780: pr f mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl
00790 L790: pr f "5,30,pic(zzzzzz)": tr(1)
00800   pr f "24,20,C 50,N": "F1 Continue   F2 verify name    F4 Search"
00810   if gx><0 then goto L910
00820 L820: input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2) conv L870
00825   if cmdkey=4 then gosub TMSRCH : goto L760
00830   p$=uprc$(lpad$(rtrm$(p$),5))
00840   if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
00850   ce=0
00860   goto L1280
00870 L870: if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
00880   ce=cnt+1
00890   fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,"U","RC")
00900   goto L820
00910 L910: if ps1=1 or vf=1 then goto L1060
00920 L920: rinput fields "3,30,C 5,EU,n": p$ conv L920
00922   if cmdkey=4 then gosub TMSRCH : goto L920
00930   p$=uprc$(lpad$(rtrm$(p$),5))
00940   if ltrm$(p$)="-1" then pr f mat otgl$: mat gln1 else pr f mat otgl$: mat gln2
00950   if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto L590
00960   if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto L1630
00970   if ltrm$(p$)="-1" then name$="CASH SALE" else goto L990
00980   goto L1050
00990 L990: read #9,using L1000,key=p$,release: name$ nokey L1020 ioerr L2290
01000 L1000: form pos 6,c 25
01010   goto L1050
01020 L1020: name$="INVALID CLIENT NUMBER"
01030   pr f "3,40,C 25,R,N": name$
01040   goto L920
01050 L1050: pr f "3,40,C 25,N": name$
01060 L1060: fli1$(4)="6,30,n 11.2,ut,n"
01070   if r1>0 then goto L1170
01080   if tr5=3 then fli1$(4)="6,30,n 11.2,ue,n"
01090   input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl conv L1240
01100   if cmdkey=2 then goto L920
01110   if tr5<>3 then goto L1200
01120   fli1$(4)="6,30,n 11.2,ut,n"
01130   if sz=4 then gl(1,2)=gln1(2): gl(1,1)=gln1(1): gl(1,3)=tr(3)
01140   if sz=3 then gl(1,1)=gln1(2): gl(1,2)=gln1(3): gl(1,3)=tr(3)
01150   if sz=2 then gl(1,2)=gln1(2): gl(1,1)=gln1(1): gl(1,3)=gln1(3): gl(1,4)=tr(3)
01160   if sz=5 then gl(1,1)=gln1(2): gl(1,2)=tr(3)
01170 L1170: pr f mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl
01180 L1180: input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl conv L1240
01190   if cmdkey=2 then goto L920
01200 L1200: p$=uprc$(lpad$(rtrm$(p$),5))
01210   if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
01220   ce=0
01230   goto L1280
01240 L1240: if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
01250   ce=cnt+1
01260   fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,"U","RC")
01270   if cnt<=4 then goto L1060 else goto L1180
01280 L1280: if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto L590
01290   if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto L1630
01300   ps1=1
01310   if tr(1)<10100 or tr(1)>123199 then 
01320     pr f "5,48,c 20": "Invalid Date"
01330     goto L790
01332   end if 
01340 L1340: if tr(3)><0 then goto L1370
01350   pr f "6,48,c 20": "NO AMOUNT ENTERED"
01360   goto L790
01370 L1370: if gx=0 then goto L1520
01380   if pgl(gpx)>0 then goto L1410
01390   pr f "9,45,c 30": "G/L # REQUIRED"
01400   goto L790
01410 L1410: gla=0
01420   for j=1 to 10
01430     if gl(j,gx)=0 then goto L1460
01440     gla=gla+gl(j,gx)
01450   next j
01460 L1460: if tr5=3 then gla=gla-tr(2)
01470   if gla=tr(3) then goto L1520
01480   pr f "11,2,c 75,h,n": " G/L allocations do not agree with total amount.  Press enter to continue."
01490   input fields "11,78,c 1,EU,n": pause$
01500   pr f "11,2,c 75,n,n": " "
01510   goto L790
01520 L1520: if ltrm$(p$)="-1" then goto L1540
01530   pt(1)=pt(1)+val(p$) conv L1540
01540 L1540: pt(tr5+1)=pt(tr5+1)+tr(3)
01550   if tr5=3 then tdt=tdt+tr(2)
01560   if ltrm$(p$)="-1" then pt(6)=pt(6)+tr(3)
01570   if vf=1 then goto L1670
01580   r3=r3+1
01590   tr(5)=tr5
01600   write #h_addr,using f3$,rec=r3: p$,iv$,mat tr,id$,mat pgl,mat gl
01605   p$=""
01610   q2=0
01620   goto L710
01630 L1630: iv$=" "
01640   mat tr=(0)
01650   id$=" "
01660   mat gl=(0)
01670 L1670: rewrite #h_addr,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl
01675   p$=""
01680   goto L2060
01690 L1690: vf=1
01700 L1700: pr newpage
01710   hd$(1)="A/R Input Proof Totals"
01720   hd$(2)=""
01730   pr f mat fl1$: mat sc3$,mat hd$
01740   pr f "11,5,C 20": "TOTAL CASH SALES"
01750   pr f "12,5,C 22": "TOTAL DISCOUNTS TAKEN"
01760   pr f mat flo3$: mat pt
01770   pr f "12,26,n 11.2": tdt
01780   pr f "18,1,C 70,H,N": "ENTER 1 TO MERGE; 2 FOR CORRECTIONS: 5 STOP WITHOUT POSTING"
01790 L1790: input fields "18,61,n 1,eu,n": j conv L1790
01795   if j=5 then goto XIT
01800   on j goto L2270,L1810 none L1790
01810 L1810: pr newpage
01820   pr f "10,5,c 60": "ENTER 1 FOR A LISTING OF ENTRIES; ELSE ENTER 2"
01830 L1830: input fields "10,60,n 1,eu,n": j conv L1830
01840   on j goto L1850,L2060 none L1830
01850 L1850: r=0
01860   fnopenprn
01870   pr newpage
01880   on fkey 5 goto L2040
01890   pr newpage
01900   pr #255,using L1910: date$,cnam$,time$,"INPUT EDIT LIST"
01910 L1910: form pos 1,c 8,pos namtab,c 50,skip 1,pos 1,c 8,pos 58,c 50,skip 1
01920   pr f "10,20,c 40,n": "INPUT EDIT LISTING IN PROCESS"
01930   pr f "23,2,C 30,N": "Press F5 to stop"
01940   pr #255: "REF #  CL #  INVOICE #";
01950   pr #255: tab(34);"Date     Amount             Description           Discount          Tr Code"
01960 L1960: r=r+1
01970   read #h_addr,using L2110,rec=r: p$,iv$,mat tr,id$ eof L2040,noRec L2040 ioerr L2290
01980   if ltrm$(p$)="0" or ltrm$(p$)="" then goto L1960
01990   name$=""
02000   read #9,using L1000,key=p$,release: name$ nokey L2010
02010 L2010: pr #255,using L2020: r,p$,iv$,tr(1),tr(3),tr(4),name$(1:22),tr(2),tr(5)
02020 L2020: form pos 1,n 4,x 2,c 5,x 2,c 18,n 6,n 11.2,pic(zzzzzz),x 7,c 22,n 12.2,n 12,skip 1
02030   goto L1960
02040 L2040: fncloseprn
02050   on fkey 5 ignore 
02060 L2060: pr newpage
02070   pr f "10,10,c 60": "ENTER REF # TO CORRECT; ENTER 0 WHEN COMPLETED"
02080 L2080: input fields "10,61,n 4,eu,n": r1 conv L2080
02090   if r1=0 then goto L2220
02100   read #h_addr,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl noRec L2060 ioerr L2290
02110 L2110: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20
02120   if ltrm$(p$)="0" or ltrm$(p$)="" then goto L2060
02130   tr5=tr(5)
02140   if p><-1 then pt(1)=pt(1)-val(p$) conv L2150
02150 L2150: pt(tr5+1)=pt(tr5+1)-tr(3)
02160   if ltrm$(p$)="-1" then pt(6)=pt(6)-tr(3)
02170   if tr5=3 then tdt=tdt-tr(2)
02180   hd$(1)="A/R CORRECT "&sc1$(tr5+1)(5:18)
02190   hd$(2)="ENTER CLIENT # AS 0 TO DELETE THIS ENTRY"
02200   vf=1
02210   goto L680
02220 L2220: pr newpage
02230   vf=0
02240   pr f "10,10,c 50": "ENTER 1 TO MAKE ADDITIONAL ENTRIES; ELSE ENTER 2"
02250 L2250: input fields "10,61,N 1,EU,N": j conv L2250
02260   on j goto L590,L1700 none L2250
02270 L2270: chain "S:\acsTM\ARMerge"
02280 XIT: pr newpage: fnxit
02290 L2290: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2310
02300   goto L2350
02310 L2310: pr newpage
02320   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2340
02330   goto L2350
02340 L2340: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
02350 L2350: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
02360   input fields "24,60,C 1,N": quitcode$
02370   if err=61 and rtrm$(uprc$(quitcode$))="Q" then goto L610 else goto L2410
02380   pr f "23,3,C 78,N": ""
02390   pr f "24,3,C 78,N": ""
02400   retry 
02410 L2410: goto XIT
04800 TMSRCH: ! search for customer #
04810   dim heading$*70,form$*80,numeric_format$*20,selection$*70
04820   file_num=11 ! alpha index on clients
04830   form$="form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2"
04840   numeric_format$='pic($$$,$$$.##)'
04850   key_length=5
04860   heading$="Acct #횼ame컴컴컴컴컴컴컴컴컴컴Address컴컴컴컴Balance"
04870   fnsearch(cap$,file_num,heading$,form$,numeric_format$,selection$,key_length)
04880   p$=selection$ ! pull key from first field in search line
04890   ano=0
04900   ano=val(selection$) conv L4910
04910 L4910: return 
