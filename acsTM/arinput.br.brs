00020   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnconsole,fnd1,fnsearch
00030   let fntop(program$,cap$="Collections")
00040   let fncno(cno,cnam$)
00050   let fnconsole(1)
00060   dim fl1$(7),flo1$(11),sc3$(5),pt(6),f3$*255,flo3$(6),name$*25
00070   dim p$*5,iv$*12,tr(6),id$*20,sc1$(5),sc2$(9),hd$(2)*50,cnam$*40
00080   dim flo4$(5),sc4$(5),ot4$(5),fli4$(5),q(3),gln1(3),gln2(3),otgl$(3)
00090   dim gl(10,4),fli1$(49),ot1$(49),pgl(3)
00100   open #h_company:=1: "Name="&env$('Q')&"\TMmstr\Company.h"&str$(cno)&",Shr",internal,input ioerr L2290
00110   read #h_company,using L130: i3,i4,i5,mat gln1,mat gln2 ioerr L2290
00120 ! Let I3=1 ! ENTER G/L #'S
00130 L130: form pos 161,3*n 1,pos 178,n 3,n 6,n 3,n 3,n 6,n 3
00140   close #h_company: 
00150   let namtab=66-int(len(rtrm$(cnam$))/2)
00160   let otgl$(1)="9,30,pic(zzz)"
00170   let otgl$(2)="9,34,pic(zzzzzz)"
00180   let otgl$(3)="9,41,pic(zzz)"
00190   if i3=0 then goto L490
00200   if i4=1 and i5=1 then goto L300
00210   if i4=0 and i5=1 then goto L350
00220   if i4=1 and i5=0 then goto L420
00230 ! NO DEPT    NO SUBACCOUNT
00240   let sz=5
00250   let gx=2
00260   mat gl(10,2)=(0)
00270   mat pgl(1)=(0)
00280   let gpx=1
00290   goto L510
00300 L300: ! YES DEPT   YES SUBACCOUNT
00310   let sz=2
00320   let gx=4
00330   let gpx=2
00340   goto L510
00350 L350: ! NO DEPT    YES SUBACCOUNT
00360   let sz=3
00370   let gx=3
00380   mat gl(10,3)=(0)
00390   mat pgl(2)=(0)
00400   let gpx=1
00410   goto L510
00420 L420: ! YES DEPT    NO SUB ACCOUNT
00430   let sz=4
00440   let gx=3
00450   mat gl(10,3)=(0)
00460   mat pgl(2)=(0)
00470   let gpx=2
00480   goto L510
00490 L490: ! NO GL TO BE ENTERED
00500   let sz=6
00510 L510: open #h_addr:=3: "Name="&env$('Temp')&"\Addr."&session$,internal,outin ioerr L530
00520   close #h_addr,free: 
00530 L530: open #h_addr:=3: "Name="&env$('Temp')&"\Addr."&session$&",SIZE=0,RecL=239",internal,outin,relative ioerr L2290
00540   open #1: "Name=S:\acsTM\TMSCRN.CL,Shr",internal,input,relative ioerr L2290
00550   read #1,using L560,rec=sz: f3$,mat fl1$,mat sc1$,mat sc2$,mat fli1$,mat ot1$,mat flo1$,mat flo3$,mat sc3$ ioerr L2290
00560 L560: form pos 1,c 255,142*c 18
00570   close #1: 
00580   open #9: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr L2290
00585   open #11: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndx2.h"&str$(cno)&",Shr",internal,input,keyed ioerr L2290
00590 L590: let hd$(1)="A/R Input Selection Menu"
00600   let hd$(2)="ENTER SELECTION"
00610 L610: print newpage
00620   print fields mat fl1$: mat sc1$,mat hd$
00630 L630: input fields "13,29,n 1,eu,n": tr5 conv L630
00640   if tr5=0 then goto L1690
00650   if tr5<1 or tr5>4 then goto L630
00660   let hd$(1)="A/R INPUT "&sc1$(tr5+1)(5:18)
00670   let hd$(2)="Client Number as 0 to stop"
00680 L680: if tr5=4 or tr5=3 then let sc2$(7)="G/L # TO CREDIT" else let sc2$(7)="G/L # TO Debit"
00690   if tr5=3 then let sc2$(6)="DISCOUNT AMOUNT" else let sc2$(6)=""
00700   if gx=0 then let sc2$(7)=" "
00710 L710: print newpage
00720   print fields mat flo1$: mat sc2$,mat hd$
00730   let ps1=0
00740   if vf=0 then goto L790
00750   if gx><0 then goto L780
00760 L760: print fields mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2)
00770   goto L790
00780 L780: print fields mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl
00790 L790: print fields "5,30,pic(zzzzzz)": tr(1)
00800   print fields "24,20,C 50,N": "F1 Continue   F2 verify name    F4 Search"
00810   if gx><0 then goto L910
00820 L820: input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2) conv L870
00825   if cmdkey=4 then gosub TMSRCH : goto L760
00830   let p$=uprc$(lpad$(rtrm$(p$),5))
00840   if ce>0 then let fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
00850   let ce=0
00860   goto L1280
00870 L870: if ce>0 then let fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
00880   let ce=cnt+1
00890   let fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,"U","RC")
00900   goto L820
00910 L910: if ps1=1 or vf=1 then goto L1060
00920 L920: rinput fields "3,30,C 5,EU,n": p$ conv L920
00922   if cmdkey=4 then gosub TMSRCH : goto L920
00930   let p$=uprc$(lpad$(rtrm$(p$),5))
00940   if ltrm$(p$)="-1" then print fields mat otgl$: mat gln1 else print fields mat otgl$: mat gln2
00950   if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto L590
00960   if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto L1630
00970   if ltrm$(p$)="-1" then let name$="CASH SALE" else goto L990
00980   goto L1050
00990 L990: read #9,using L1000,key=p$,release: name$ nokey L1020 ioerr L2290
01000 L1000: form pos 6,c 25
01010   goto L1050
01020 L1020: let name$="INVALID CLIENT NUMBER"
01030   print fields "3,40,C 25,R,N": name$
01040   goto L920
01050 L1050: print fields "3,40,C 25,N": name$
01060 L1060: let fli1$(4)="6,30,n 11.2,ut,n"
01070   if r1>0 then goto L1170
01080   if tr5=3 then let fli1$(4)="6,30,n 11.2,ue,n"
01090   input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl conv L1240
01100   if cmdkey=2 then goto L920
01110   if tr5<>3 then goto L1200
01120   let fli1$(4)="6,30,n 11.2,ut,n"
01130   if sz=4 then let gl(1,2)=gln1(2): let gl(1,1)=gln1(1): let gl(1,3)=tr(3)
01140   if sz=3 then let gl(1,1)=gln1(2): let gl(1,2)=gln1(3): let gl(1,3)=tr(3)
01150   if sz=2 then let gl(1,2)=gln1(2): let gl(1,1)=gln1(1): let gl(1,3)=gln1(3): let gl(1,4)=tr(3)
01160   if sz=5 then let gl(1,1)=gln1(2): let gl(1,2)=tr(3)
01170 L1170: print fields mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl
01180 L1180: input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl conv L1240
01190   if cmdkey=2 then goto L920
01200 L1200: let p$=uprc$(lpad$(rtrm$(p$),5))
01210   if ce>0 then let fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
01220   let ce=0
01230   goto L1280
01240 L1240: if ce>0 then let fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
01250   let ce=cnt+1
01260   let fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,"U","RC")
01270   if cnt<=4 then goto L1060 else goto L1180
01280 L1280: if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto L590
01290   if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto L1630
01300   let ps1=1
01310   if tr(1)<10100 or tr(1)>123199 then 
01320     print fields "5,48,c 20": "Invalid Date"
01330     goto L790
01332   end if 
01340 L1340: if tr(3)><0 then goto L1370
01350   print fields "6,48,c 20": "NO AMOUNT ENTERED"
01360   goto L790
01370 L1370: if gx=0 then goto L1520
01380   if pgl(gpx)>0 then goto L1410
01390   print fields "9,45,c 30": "G/L # REQUIRED"
01400   goto L790
01410 L1410: let gla=0
01420   for j=1 to 10
01430     if gl(j,gx)=0 then goto L1460
01440     let gla=gla+gl(j,gx)
01450   next j
01460 L1460: if tr5=3 then let gla=gla-tr(2)
01470   if gla=tr(3) then goto L1520
01480   print fields "11,2,c 75,h,n": " G/L allocations do not agree with total amount.  Press enter to continue."
01490   input fields "11,78,c 1,EU,n": pause$
01500   print fields "11,2,c 75,n,n": " "
01510   goto L790
01520 L1520: if ltrm$(p$)="-1" then goto L1540
01530   let pt(1)=pt(1)+val(p$) conv L1540
01540 L1540: let pt(tr5+1)=pt(tr5+1)+tr(3)
01550   if tr5=3 then let tdt=tdt+tr(2)
01560   if ltrm$(p$)="-1" then let pt(6)=pt(6)+tr(3)
01570   if vf=1 then goto L1670
01580   let r3=r3+1
01590   let tr(5)=tr5
01600   write #h_addr,using f3$,rec=r3: p$,iv$,mat tr,id$,mat pgl,mat gl
01605   let p$=""
01610   let q2=0
01620   goto L710
01630 L1630: let iv$=" "
01640   mat tr=(0)
01650   let id$=" "
01660   mat gl=(0)
01670 L1670: rewrite #h_addr,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl
01675   let p$=""
01680   goto L2060
01690 L1690: let vf=1
01700 L1700: print newpage
01710   let hd$(1)="A/R Input Proof Totals"
01720   let hd$(2)=""
01730   print fields mat fl1$: mat sc3$,mat hd$
01740   print fields "11,5,C 20": "TOTAL CASH SALES"
01750   print fields "12,5,C 22": "TOTAL DISCOUNTS TAKEN"
01760   print fields mat flo3$: mat pt
01770   print fields "12,26,n 11.2": tdt
01780   print fields "18,1,C 70,H,N": "ENTER 1 TO MERGE; 2 FOR CORRECTIONS: 5 STOP WITHOUT POSTING"
01790 L1790: input fields "18,61,n 1,eu,n": j conv L1790
01795   if j=5 then goto XIT
01800   on j goto L2270,L1810 none L1790
01810 L1810: print newpage
01820   print fields "10,5,c 60": "ENTER 1 FOR A LISTING OF ENTRIES; ELSE ENTER 2"
01830 L1830: input fields "10,60,n 1,eu,n": j conv L1830
01840   on j goto L1850,L2060 none L1830
01850 L1850: let r=0
01860   let fnopenprn
01870   print newpage
01880   on fkey 5 goto L2040
01890   print newpage
01900   print #255,using L1910: date$,cnam$,time$,"INPUT EDIT LIST"
01910 L1910: form pos 1,c 8,pos namtab,c 50,skip 1,pos 1,c 8,pos 58,c 50,skip 1
01920   print fields "10,20,c 40,n": "INPUT EDIT LISTING IN PROCESS"
01930   print fields "23,2,C 30,N": "Press F5 to stop"
01940   print #255: "REF #  CL #  INVOICE #";
01950   print #255: tab(34);"Date     Amount             Description           Discount          Tr Code"
01960 L1960: let r=r+1
01970   read #h_addr,using L2110,rec=r: p$,iv$,mat tr,id$ eof L2040,norec L2040 ioerr L2290
01980   if ltrm$(p$)="0" or ltrm$(p$)="" then goto L1960
01990   let name$=""
02000   read #9,using L1000,key=p$,release: name$ nokey L2010
02010 L2010: print #255,using L2020: r,p$,iv$,tr(1),tr(3),tr(4),name$(1:22),tr(2),tr(5)
02020 L2020: form pos 1,n 4,x 2,c 5,x 2,c 18,n 6,n 11.2,pic(zzzzzz),x 7,c 22,n 12.2,n 12,skip 1
02030   goto L1960
02040 L2040: let fncloseprn
02050   on fkey 5 ignore 
02060 L2060: print newpage
02070   print fields "10,10,c 60": "ENTER REF # TO CORRECT; ENTER 0 WHEN COMPLETED"
02080 L2080: input fields "10,61,n 4,eu,n": r1 conv L2080
02090   if r1=0 then goto L2220
02100   read #h_addr,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl norec L2060 ioerr L2290
02110 L2110: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20
02120   if ltrm$(p$)="0" or ltrm$(p$)="" then goto L2060
02130   let tr5=tr(5)
02140   if p><-1 then let pt(1)=pt(1)-val(p$) conv L2150
02150 L2150: let pt(tr5+1)=pt(tr5+1)-tr(3)
02160   if ltrm$(p$)="-1" then let pt(6)=pt(6)-tr(3)
02170   if tr5=3 then let tdt=tdt-tr(2)
02180   let hd$(1)="A/R CORRECT "&sc1$(tr5+1)(5:18)
02190   let hd$(2)="ENTER CLIENT # AS 0 TO DELETE THIS ENTRY"
02200   let vf=1
02210   goto L680
02220 L2220: print newpage
02230   let vf=0
02240   print fields "10,10,c 50": "ENTER 1 TO MAKE ADDITIONAL ENTRIES; ELSE ENTER 2"
02250 L2250: input fields "10,61,N 1,EU,N": j conv L2250
02260   on j goto L590,L1700 none L2250
02270 L2270: chain "S:\acsTM\ARMerge"
02280 XIT: print newpage: let fnxit
02290 L2290: if err=61 then print fields "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2310
02300   goto L2350
02310 L2310: print newpage
02320   if err=4148 then print fields "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2340
02330   goto L2350
02340 L2340: print fields "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
02350 L2350: print fields "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
02360   input fields "24,60,C 1,N": quitcode$
02370   if err=61 and rtrm$(uprc$(quitcode$))="Q" then goto L610 else goto L2410
02380   print fields "23,3,C 78,N": ""
02390   print fields "24,3,C 78,N": ""
02400   retry 
02410 L2410: goto XIT
04800 TMSRCH: ! search for customer #
04810   dim heading$*70,form$*80,numeric_format$*20,selection$*70
04820   let file_num=11 ! alpha index on clients
04830   let form$="form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2"
04840   let numeric_format$='pic($$$,$$$.##)'
04850   let key_length=5
04860   let heading$="Acct #횼ame컴컴컴컴컴컴컴컴컴컴Address컴컴컴컴Balance"
04870   let fnsearch(cap$,file_num,heading$,form$,numeric_format$,selection$,key_length)
04880   let p$=selection$ ! pull key from first field in search line
04890   let ano=0
04900   let ano=val(selection$) conv L4910
04910 L4910: return 
