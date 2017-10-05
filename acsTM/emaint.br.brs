00010 ! REPLACE S:\acsTM\EMAINT
00020 ! 
00030   on error goto L2380
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnconsole,fndat
00050   fntop(program$,cap$="Employee")
00060   fnconsole(1)
00070   fncno(cno,cnam$)
00075   fndat(dat$)
00080 ! 
00100   dim scr3$(15)*25,fl3$(17),ot3$(15),in3$(15),scrid3$(2)*80
00110   dim fl4$(23),ot4$(40),in4$(40),scrid4$(3)*80
00120   dim fl5$(22),ot5$(40),in5$(40),scrid5$(2)*80
00130   dim fl6$(22),ot6$(40),in6$(40),scrid6$(2)*80
00140   dim s(10),t(10),u(10),v(10),w(20),x(20),y(20),z(20),e(4,30)
00150   dim eno$*9,e$*25,d(2),r(11),ccat$(10)*30,nccat$(20)*30,tcat$*20
00160   dim hlp$(20)*78,flh$(22)*18,hhd$*60,cnam$*40,dat$*20,a$*5
00170   open #1: "Name=S:\acsTM\TMSCRN.EM,Shr",internal,input ioerr L2380
00180 L180: form pos 1,103*c 18
00190   read #1,using L180: mat fl3$,mat ot3$,mat in3$ ioerr L2380
00200   read #1,using L180: mat fl4$,mat ot4$,mat in4$ ioerr L2380
00210   read #1,using L180: mat fl5$,mat ot5$,mat in5$ ioerr L2380
00220   read #1,using L180: mat fl6$,mat ot6$,mat in6$ ioerr L2380
00230   close #1: 
00240   open #1: "Name="&env$('Q')&"\TMmstr\TMCat.h"&str$(cno)&",Shr",internal,input,relative ioerr L2380
00250   read #1,using L260: mat ccat$,mat nccat$ ioerr L2380
00260 L260: form pos 1,30*c 30
00270   close #1: 
00280   gosub L1270
00290   open #1: "Name="&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\EMIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L2370
00300 L300: pr newpage
00310   on fkey 5 goto L300
00320   pr f "3,9,C 60": "Employee Master File"
00330   pr f "4,6,C 72": "Company Number "&str$(cno)&"  "&ltrm$(cnam$)
00340   pr f "6,9,C 60": "1 = Initial File Preparation"
00350   pr f "7,9,C 60": "2 = Add New Records"
00360   pr f "8,9,C 60": "3 = File Maintenance / Inquiry"
00370   pr f "9,9,C 60": "4 = pr Proof Listing"
00380   pr f "10,9,C 60": "5 = Staff Directory Report"
00390   pr f "11,9,C 60": "6 = Remove Terminated Employees"
00400   pr f "13,9,C 60": "0 = Completed (Return To T/M Menu)"
00410   pr f "15,9,C 18": "Enter Selection #:"
00420 L420: input fields "15,35,N 1,UE,N": ti conv L420
00430   if ti=0 then goto L2190
00440 L440: on ti goto L2220,L470,L520,L1720,L450,L460 none L300
00450 L450: chain "S:\acsTM\TMSTAFDR"
00460 L460: chain "S:\acsTM\EMREMTER"
00470 L470: scrid3$(1)="* TIME MANAGEMENT ADD EMPLOYEE RECORDS * SCREEN 1 * "&date$&" * "&time$
00480   scrid3$(2)="  ENTER EMPLOYEE # AS 0 WHEN COMPLETED"
00490   mat d=(0): mat s=(0): mat w=(0): mat t=(0): mat x=(0): mat u=(0): mat y=(0): mat v=(0): mat z=(0): mat r=(0)
00500   eno$=" ": e$=" "
00510   goto L630
00520 L520: scrid3$(1)="* TIME MANAGEMENT MAINTAIN EMPLOYEES * SCREEN 1 * "&date$&" * "&time$
00530   scrid3$(2)="  ENTER EMPLOYEE # AS 0 TO DELETE"
00540 L540: pr newpage
00550   pr f "10,10,c 53,n": "ENTER EMPLOYEE NUMBER, ENTER 0 WHEN COMPLETE"
00560 L560: input fields "10,60,N 9,UE,N": eno conv L560
00570   if eno=0 then goto L300
00580   eno$=lpad$(str$(eno),9)
00590   read #1,using L600,key=eno$: eno$,e$,mat d,mat s,mat w,mat t,mat x,mat u,mat y,mat v,mat z,mat r nokey L540 ioerr L2380
00600 L600: form pos 1,c 9,c 25,pd 2,n 1,60*pd 4.2,60*pd 5.2,11*pd 3.2
00610   eno=val(eno$)
00620   holdeno$=eno$
00630 L630: pr newpage
00640   pr f mat fl3$: mat scr3$,mat scrid3$
00650   pr f mat ot3$: eno$,e$,mat d,mat r
00660   pr f "24,1,C 80,H,N": "F1=SCREEN 1, F2=SCREEN 2, F3=SCREEN 3, F4=SCREEN 4, F5=COMPLETED"
00670 L670: input fields mat in3$: x1,e$,mat d,mat r conv L1400
00680   if cv>0 then in3$(cv)(cv1:cv2)="U": cv=0
00690   if ti=2 and x1=0 then goto L300
00700   eno$=lpad$(str$(x1),9)
00710   if x1=eno or ti>2 then goto L880
00720   if x1=0 then goto L790
00730   eno$=lpad$(str$(x1),9)
00740   read #1,using L600,key=eno$: eno$ nokey L880 ioerr L2380
00750   pr f "4,38,c 38,r,n": "DUPLICATE EMPLOYEE; ENTER to continue."
00760   input fields "4,78,c 1,ae,n": pause$
00770   pr f "4,38,c 38,h,n": " "
00780   goto L1260
00790 L790: pr newpage
00800   pr f "10,10,c 45,n": "EMPLOYEE NUMBER "&holdeno$&" WILL BE DELETED"
00810   pr f "11,10,c 45,n": "ENTER 1 TO DELETE, ENTER 2 TO RE-ENTER"
00820 L820: input fields "11,60,N 1,UE,N": ans conv L820
00830   if ans=2 then goto L1260
00840   if ans><1 then goto L790
00850   delete #1,key=holdeno$: nokey L870
00860   new1=1
00870 L870: goto L1260
00880 L880: on cmdkey goto L630,L890,L990,L1080,L1170 none L890
00890 L890: scrid4$(1)="EMPLOYEE:"&ltrm$(eno$)&" * "&rtrm$(e$)&" * SCREEN 2 * "&date$&" * "&time$
00900   scrid4$(2)="                        CURRENT CHARGEABLE HOURS     YTD CHARGEABLE HOURS"
00910   scrid4$(3)="                       CURRENT CHARGEABLE AMOUNT     YTD CHARGEABLE AMOUNT"
00920   pr newpage
00930   pr f mat fl4$: mat ccat$,mat ccat$,mat scrid4$
00940   pr f mat ot4$: mat s,mat t,mat u,mat v
00950   pr f "24,1,C 80,H,N": "F1=SCREEN 1, F2=SCREEN 2, F3=SCREEN 3, F4=SCREEN 4, F5=COMPLETED"
00960 L960: input fields mat in4$: mat s,mat t,mat u,mat v conv L1480
00970   if cv>0 then in4$(cv)(cv1:cv2)="U": cv=0
00980   on cmdkey goto L630,L890,L990,L1080,L1170 none L990
00990 L990: scrid5$(1)="EMPLOYEE:"&ltrm$(eno$)&" * "&rtrm$(e$)&" * SCREEN 3 * "&date$&" * "&time$
01000   scrid5$(2)="           NON-CHARGEABLE HOURS - CURRENT           NON-CHARGEABLE HOURS- YTD"
01010   pr newpage
01020   pr f mat fl5$: mat nccat$,mat scrid5$
01030   pr f mat ot5$: mat w,mat x
01040   pr f "24,1,C 80,H,N": "F1=SCREEN 1, F2=SCREEN 2, F3=SCREEN 3, F4=SCREEN 4, F5=COMPLETED"
01050 L1050: input fields mat in5$: mat w,mat x conv L1560
01060   if cv>0 then in5$(cv)(cv1:cv2)="U": cv=0
01070   on cmdkey goto L630,L890,L990,L1080,L1170 none L1080
01080 L1080: scrid6$(1)="EMPLOYEE:"&ltrm$(eno$)&" * "&rtrm$(e$)&" * SCREEN 4 * "&date$&" * "&time$
01090   scrid6$(2)="         NON-CHARGEABLE AMOUNT - CURRENT           NON-CHARGEABLE AMOUNT - YTD"
01100   pr newpage
01110   pr f mat fl6$: mat nccat$,mat scrid6$
01120   pr f mat ot6$: mat y,mat z
01130   pr f "24,1,C 80,H,N": "F1=SCREEN 1, F2=SCREEN 2, F3=SCREEN 3, F4=SCREEN 4, F5=COMPLETED"
01140 L1140: input fields mat in6$: mat y,mat z conv L1640
01150   if cv>0 then in6$(cv)(cv1:cv2)="U": cv=0
01160   on cmdkey goto L630,L890,L990,L1080,L1170 none L1170
01170 L1170: if ti=2 then goto L1200
01180   if x1=eno then goto L1250
01190   delete #1,key=holdeno$: 
01200 L1200: write #1,using L600: eno$,e$,mat d,mat s,mat w,mat t,mat x,mat u,mat y,mat v,mat z,mat r
01210   mat d=(0): mat s=(0): mat w=(0): mat t=(0): mat x=(0): mat u=(0): mat y=(0): mat v=(0): mat z=(0): mat r=(0)
01220   eno$=" ": e$=" "
01230   new1=1
01240   goto L1260
01250 L1250: rewrite #1,using L600,key=eno$: eno$,e$,mat d,mat s,mat w,mat t,mat x,mat u,mat y,mat v,mat z,mat r
01260 L1260: goto L440
01270 L1270: scr3$(1)="EMPLOYEE NUMBER"
01280   scr3$(2)="EMPLOYEE NAME"
01290   scr3$(3)="PRIMARY DEPARTMENT #"
01300   scr3$(4)="EMPLOYEE STATUS"
01310   for j=1 to 10
01320     if rtrm$(ccat$(j))="" then goto L1360
01330     tcat$=rtrm$(ccat$(j)(1:20))
01340     scr3$(j+4)=tcat$&" RATE"
01350     goto L1370
01360 L1360: scr3$(j+4)=" "
01370 L1370: next j
01380   scr3$(15)="NON - CHARGEABLE RATE"
01390   return 
01400 L1400: if cv>0 then in3$(cv)(cv1:cv2)="U"
01410   cv=cnt+1
01420   pr f "24,78,C 1": bell
01430   in3$(cv)=rtrm$(in3$(cv))
01440   cv1=pos(uprc$(in3$(cv)),"U",1)
01450   cv2=cv1+1
01460   in3$(cv)(cv1:cv1)="CR"
01470   goto L670
01480 L1480: if cv>0 then in4$(cv)(cv1:cv2)="U"
01490   cv=cnt+1
01500   pr f "24,78,C 1": bell
01510   in4$(cv)=rtrm$(in4$(cv))
01520   cv1=pos(uprc$(in4$(cv)),"U",1)
01530   cv2=cv1+1
01540   in4$(cv)(cv1:cv1)="RC"
01550   goto L960
01560 L1560: if cv>0 then in5$(cv)(cv1:cv2)="U"
01570   cv=cnt+1
01580   pr f "24,78,C 1": bell
01590   in5$(cv)=rtrm$(in5$(cv))
01600   cv1=pos(uprc$(in5$(cv)),"U",1)
01610   cv2=cv1+1
01620   in5$(cv)(cv1:cv1)="RC"
01630   goto L1050
01640 L1640: if cv>0 then in6$(cv)(cv1:cv2)="U"
01650   cv=cnt+1
01660   pr f "24,78,C 1": bell
01670   in6$(cv)=rtrm$(in6$(cv))
01680   cv1=pos(uprc$(in6$(cv)),"U",1)
01690   cv2=cv1+1
01700   in6$(cv)(cv1:cv1)="RC"
01710   goto L1140
01720 L1720: pr newpage
01725   fnopenprn
01730   pr f "7,15,c 50,h,n": "POSITION PAPER FOR EMPLOYEE PROOF LISTING"
01740   pr f "10,10,c 40,n": "ENTER AS OF DATE FOR EMPLOYEE PROOF LIST"
01750   pr f "10,52,c 20,n": dat$
01760 L1760: input fields "10,52,c 20,eu,n": dat$ conv L1760
01770   dattab=66-int(len(rtrm$(dat$))/2)
01780   pr newpage
01790   on fkey 5 goto L2140
01800   pr f "10,20,c 50,h,n": "EMPLOYEE PROOF LIST IN PROCESS"
01810   pr f "23,2,C 60": "Press F5 to STOP"
01820   restore #1,key>="         ": nokey L2150
01830 L1830: read #1,using L1840: eno$,e$,mat d,mat e,mat r eof L2150 ioerr L2380
01840 L1840: form pos 1,c 9,c 25,pd 2,n 1,60*pd 4.2,60*pd 5.2,11*pd 3.2
01850   gosub L2160
01860   pr #255,using L1870: "EMPLOYEE NUMBER",eno$
01870 L1870: form pos 1,c 20,pos 25,c 9,skip 1
01880   pr #255,using L1890: "EMPLOYEE NAME",e$
01890 L1890: form pos 1,c 20,pos 25,c 25,skip 1
01900   pr #255,using L1910: "PRIMARY DEPARTMENT #",d(1)
01910 L1910: form pos 1,c 20,pos 25,n 4,skip 1
01920   pr #255,using L1930: "EMPLOYEE STATUS",d(2)
01930 L1930: form pos 1,c 20,pos 25,n 1,skip 1
01940   pr #255,using L1950: "NON-CHARGEABLE RATE",r(11)
01950 L1950: form pos 1,c 20,pos 25,n 6.2,skip 5
01960   pr #255,using L1970: "CHARGEABLE HOURS","CHARGEABLE AMOUNT"
01970 L1970: form pos 39,c 16,pos 76,c 17,skip 1
01980   pr #255,using L1990: "CURRENT            YTD","CURRENT            YTD","RATES"
01990 L1990: form pos 36,c 25,pos 73,c 25,pos 100,c 5,skip 2
02000   for j=1 to 10
02010     pr #255,using L2020: ccat$(j),e(1,j),e(2,j),e(3,j),e(4,j),r(j)
02020 L2020: form pos 1,c 30,pos 35,n 8.2,pos 50,n 8.2,pos 70,n 10.2,pos 85,n 10.2,pos 99,n 6.2,skip 1
02030   next j
02040   pr #255,using L2050: "NON-CHARGEABLE HOURS","NON-CHARGEABLE AMOUNT"
02050 L2050: form skip 3,pos 37,c 20,pos 73,c 21,skip 1
02060   pr #255,using L2070: "CURRENT            YTD","CURRENT            YTD"
02070 L2070: form pos 36,c 25,pos 73,c 25,skip 2
02080   for j=1 to 20
02090     pr #255,using L2100: nccat$(j),e(1,j+10),e(2,j+10),e(3,j+10),e(4,j+10)
02100 L2100: form pos 1,c 30,pos 35,n 8.2,pos 50,n 8.2,pos 70,n 10.2,pos 85,n 10.2,skip 1
02110   next j
02120   pr #255: newpage
02130   goto L1830
02140 L2140: pr #255: newpage
02150 L2150: fncloseprn
02152   goto L300
02160 L2160: pr #255,using L2161: cnam$
02161 L2161: form pos 47,cc 40,skip 1
02162   pr #255,using L2170: "EMPLOYEE PROOF LISTING",dat$
02170 L2170: form pos 56,c 26,skip 1,pos dattab,c 34,skip 2
02180   return 
02190 L2190: close #1: 
02200   if new1=1 then goto L2340
02210   goto XIT
02220 L2220: pr newpage
02230   pr f "8,22,c 31,r,n": "  ********  WARNING  ********"
02240   pr f "11,10,c 70": "THIS SELECTION ERASES ALL EMPLOYEE RECORDS ON THIS FILE."
02250   pr f "13,5,c 72": "ENTER PASSWORD TO CONTINUE; ELSE PRESS ENTER TO RETURN TO EMPLOYEE MENU."
02260 L2260: input fields "15,10,C 5,IE,N": a$ conv L2260
02270   if uprc$(a$)="THINK" then goto L2280 else goto L300
02280 L2280: pr newpage
02290 L2290: close #1: ioerr L2300
02300 L2300: open #1: "Name="&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno),internal,outin ioerr L2320
02310   close #1,free: 
02320 L2320: open #1: "Name="&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&",SIZE=0,RecL=610",internal,output ioerr L2380
02330   close #1: 
02340 L2340: execute "Index "&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&","&env$('Q')&"\TMmstr\EMIndex.h"&str$(cno)&",1,9,REPLACE,DupKeys"
02350   if ti<2 then chain 'S:\acsTM\EMAINT' else goto XIT
02360   goto XIT
02370 L2370: if err=4152 then goto L2290
02380 L2380: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2400
02390   goto L2440
02400 L2400: pr newpage
02410   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2430
02420   goto L2440
02430 L2430: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
02440 L2440: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
02450   input fields "24,60,C 1,N": quitcode$
02460   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
02470   pr f "23,3,C 78,N": ""
02480   pr f "24,3,C 78,N": ""
02490   retry 
02500 XIT: fnxit
