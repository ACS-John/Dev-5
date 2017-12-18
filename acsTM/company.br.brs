00020 ! 
00030   on error goto L750
00040   library 'S:\Core\Library': fncno,fnxit,fntop,fncloseprn,fnopenprn,fnconsole
00050   fntop(program$,cap$="Company Information")
00060   fncno(cno,cnam$)
00065   fnconsole(1)
00070   dim co$(4)*40,co(6),ag(4),dat$*20,gln(4,3)
00080   dim fl1$(20),sc1$(19)*256,io1$(27),hd1$*50,cap$*128,cnam$*40
00090 ! 
00100   data COMPANY NAME
00110   data CO ADDRESS 1
00120   data CO ADDRESS 2
00130   data CITY STATE ZIP
00140   data GL INSTALLED                                             (1=YES)
00150   data FUND NUMBER USED                                         (1=YES)
00160   data SUB ACCOUNT USED                                         (1=YES)
00170   data pr DETAILS ON STATEMENTS                              (1=YES)
00180   data FINANCE CHARGE RATE                        (ENTER 10% AS 10.000)
00190   data # OF DAYS BEFORE FIN CHG APPLICABLE
00200   data AGING PERIOD 1
00210   data AGING PERIOD 2
00220   data AGING PERIOD 3
00230   data AGING PERIOD 4
00240   data GL # CASH
00250   data GL # AR
00260   data GL # FINANCE CHARGE
00270   data GL # STANDARD CHARGE
00280   data REPORT HEADING DATE
00290   read mat sc1$
00300   for j=1 to 19
00310     fl1$(j)=str$(j+2)&",2,C 66"
00320     if j<5 then io1$(j)=str$(j+2)&",20,C 40,U,N"
00330     if j>4 and j<9 then io1$(j)=str$(j+2)&",42,N 1,U,N"
00340     if j>9 and j<15 then io1$(j)=str$(j+2)&",40,N 3,U,N"
00350   next j
00360   fl1$(20)="1,5,C 60,H,N"
00370   io1$(9)="11,37,N 6.3,U,N"
00380   for j=1 to 4
00390     io1$(j*3+12)=str$(j+16)&",30,N 3,U,N"
00400     io1$(j*3+13)=str$(j+16)&",37,N 6,U,N"
00410     io1$(j*3+14)=str$(j+16)&",47,N 3,U,N"
00420   next j
00430   io1$(27)="21,30,C 20,U,N"
00440   hd1$="TIME MANAGEMENT CHANGE COMPANY INFORMATION"
00450   open #1: "Name="&env$('Q')&"\TMmstr\Company.h"&env$('cno'),internal,outin,relative ioerr L480
00460   goto L500
00470   close #1,free: 
00480 L480: open #1: "Name="&env$('Q')&"\TMmstr\Company.h"&env$('cno')&",Replace,RecL=245",internal,outin,relative ioerr L750
00490   write #1,using L510,rec=1: mat co$,mat co,mat ag,mat gln,dat$
00500 L500: read #1,using L510,rec=1: mat co$,mat co,mat ag,mat gln,dat$ ioerr L750
00510 L510: form pos 1,4*c 40,4*n 1,pd 3.3,5*pd 2,n 3,n 6,n 3,n 3,n 6,n 3,n 3,n 6,n 3,n 3,n 6,n 3,c 20
00520   pr newpage
00530   pr f mat fl1$: mat sc1$,hd1$
00540   pr f mat io1$: mat co$,mat co,mat ag,mat gln,dat$
00545   pr f "23,30,c 40": "Enter to Save   F5 to cancel"
00550 L550: input fields mat io1$: mat co$,mat co,mat ag,mat gln,dat$ conv L670
00555   if cmdkey=5 then goto XIT
00560   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
00570   if rtrm$(co$(1))="" then ce=1: goto L690
00580   for j=1 to 4
00590     if co(j)<0 or co(j)>1 then ce=j+4: goto L690
00600     if j=1 then goto L620
00610     if ag(j-1)>ag(j) then ce=j+10: goto L690
00620 L620: next j
00630   if co(5)<0 or co(5)>99 then ce=9: goto L690
00640   rewrite #1,using L510,rec=1: mat co$,mat co,mat ag,mat gln,dat$
00650   close #1: 
00660   goto XIT
00670 L670: if ce>0 then io1$(ce)(ce1:ce2)="U"
00680   ce=cnt+1
00690 L690: pr f "24,78,C 1": bell
00700   io1$(ce)=rtrm$(io1$(ce))
00710   ce1=pos(uprc$(io1$(ce)),"U",1)
00720   ce2=ce1+1
00730   io1$(ce)(ce1:ce1)="CR"
00740   goto L550
00750 L750: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L770
00760   goto L810
00770 L770: pr newpage
00780   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L800
00790   goto L810
00800 L800: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00810 L810: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00820   input fields "24,60,C 1,N": quitcode$
00830   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
00840   pr f "23,3,C 78,N": ""
00850   pr f "24,3,C 78,N": ""
00860   retry 
00870 XIT: fnxit
