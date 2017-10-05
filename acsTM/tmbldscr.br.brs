00020 ! 
00030   dim fl1$(33),oi1$(38)
00040   dim fl2$(15),ot2$(14)
00050   dim fl3$(17),ot3$(15),in3$(15)
00060   dim fl4$(23),ot4$(40),in4$(40)
00070   dim fl6$(22),ot6$(40),in6$(40)
00080   dim fl5$(22),ot5$(40),in5$(40)
00090   dim scr1$(31)*20,scr2$(14)*25,scrid$(2)*60
00100   data "CLIENT NUMBER"
00110   data "CLIENT NAME"
00120   data "CURRENT ADDRESS"
00130   data "CITY, STATE ZIP"
00140   data "CONTACT NAME"
00150   data "TYPE OF BUSINESS"
00160   data "BUSINESS PHONE #"
00170   data "FEDERAL ID # OR SS #"
00180   data "PARTNER NUMBER"
00190   data "MONTH OF YEAR-END"
00200   data HOME PHONE #
00210   data SPOUSE SS #
00220   data CURRENT BALANCE
00230   data STANDARD CHARGE
00240   data FINANCE CHARGE
00250   data STATEMENT CODE
00260   data TYPE (1=BF 2=OI)
00270   data "COMMENT:"
00280   data "CATEGORY"
00290   data "DUE DATE"
00300   data "STATUS"
00310   data 1,2,3,4,5,6,7,8,9,10
00320   read mat scr1$ ioerr L2000
00330   data "CLIENT NUMBER"
00340   data "EMPLOYEE #"
00350   data "HOURS"
00360   data "HOURLY RATE"
00370   data "AMOUNT"
00380   data "TRANSACTION DATE"
00390   data "CATEGORY CODE"
00400   data "DEPARTMENT CODE"
00410   data "TRANSACTION CODE"
00420   data "SUB CATEGORY"
00430   data "SERVICE CODE"
00440   data "INVOICE #"
00450   data "NEXT TRANSACTION ADDRESS"
00460   data DESCRIPTION
00470   read mat scr2$ ioerr L2000
00480   for j=1 to 21
00490     if j<15 then fl2$(j)=str$(j+3)&",2,C 25,N"
00500     fl1$(j)=str$(j+1)&",2,C 25,N"
00510     if j=1 then oi1$(j)=str$(j+1)&",28,N 5,ut,N"
00520     if j>1 and j<7 then oi1$(j)=str$(j+1)&",28,C 30,ut,N"
00530     if j=7 or j=11 then oi1$(j)=str$(j+1)&",28,C 12,ut,N"
00540     if j=8 or j=12 then oi1$(j)=str$(j+1)&",28,c 11,ut,N"
00550     if j=9 then oi1$(j)=str$(j+1)&",28,n 9,ut,N"
00560     if j=10 then oi1$(j)=str$(j+1)&",28,n 2,ut,N"
00570     if j=13 or j=14 then oi1$(j)=str$(j+1)&",28,n 10.2,ut,N"
00580     if j=15 then oi1$(j)=str$(j+1)&",28,N 8.3,Ut,N"
00590     if j=16 or j=17 then oi1$(j)=str$(j+1)&",28,N 1,Ut,N"
00600     if j=18 then oi1$(j)=str$(j+1)&",10,C 70,Ut,N"
00610   next j
00620   for j=1 to 10
00630     scr1$(j+21)=lpad$(scr1$(j+21),4)
00640     fl1$(j+21)="20,"&str$(j*7+6)&",C 4,R,N"
00650     oi1$(j+18)="21,"&str$(j*7+6)&",N 4,Ut,N"
00660     oi1$(j+28)="22,"&str$(j*7+9)&",N 1,Ut,N"
00670   next j
00680   fl1$(32)="1,20,C 40,R,n"
00690   fl1$(33)="2,36,C 40,N"
00700   fl2$(15)="1,20,C 50,H,N"
00710   ot2$(1)="4,28,c 5,ut,n"
00720   ot2$(2)="5,28,c 9,ut,n"
00730   ot2$(3)="6,28,n 6.2,ut,n"
00740   ot2$(4)="7,28,n 6.2,ut,n"
00750   ot2$(5)="8,28,n 10.2,ut,n"
00760   ot2$(6)="9,28,n 6,ut,n"
00770   ot2$(7)="10,28,n 2,ut,n"
00780   ot2$(8)="11,28,n 3,ut,n"
00790   ot2$(9)="12,28,n 2,ut,n"
00800   ot2$(10)="13,28,n 2,ut,n"
00810   ot2$(11)="14,28,c 4,ut,n"
00820   ot2$(12)="15,28,c 12,ut,n"
00830   ot2$(13)="16,28,n 5,ut,n"
00840   ot2$(14)="17,28,C 30,ut,N"
00850   for j=1 to 15
00860     fl3$(j)=str$(j+3)&",2,C 25,N"
00870     if j>1 then goto L910
00880     ot3$(j)=str$(j+3)&",28,c 9,ut,N"
00890     in3$(j)=str$(j+3)&",28,n 9,ut,N"
00900     goto L1050
00910 L910: if j>2 then goto L950
00920     ot3$(j)=str$(j+3)&",28,c 25,ut,N"
00930     in3$(j)=ot3$(j)
00940     goto L1050
00950 L950: if j>3 then goto L990
00960     ot3$(j)=str$(j+3)&",28,n 3,ut,N"
00970     in3$(j)=ot3$(j)
00980     goto L1050
00990 L990: if j>4 then goto L1030
01000     ot3$(j)=str$(j+3)&",28,n 1,ut,N"
01010     in3$(j)=ot3$(j)
01020     goto L1050
01030 L1030: ot3$(j)=str$(j+3)&",28,n 6.2,ut,N"
01040     in3$(j)=str$(j+3)&",28,n 6.2,ut,N"
01050 L1050: next j
01060   fl3$(16)="1,1,C 80,H,N"
01070   fl3$(17)="2,1,C 80,H,N"
01080   for j=1 to 20
01090     if j>10 then goto L1160
01100     fl4$(j)=str$(j+2)&",2,c 30,n"
01110     ot4$(j)=str$(j+2)&",35,n 8.2,ut,n"
01120     in4$(j)=str$(j+2)&",35,n 8.2,ut,n"
01130     ot4$(j+10)=str$(j+2)&",60,n 8.2,ut,n"
01140     in4$(j+10)=str$(j+2)&",60,n 8.2,ut,n"
01150     goto L1210
01160 L1160: fl4$(j)=str$(j+3)&",2,c 30,n"
01170     ot4$(j+10)=str$(j+3)&",35,n 10.2,ut,n"
01180     in4$(j+10)=str$(j+3)&",35,n 10.2,ut,n"
01190     ot4$(j+20)=str$(j+3)&",60,n 10.2,ut,n"
01200     in4$(j+20)=str$(j+3)&",60,n 10.2,ut,n"
01210 L1210: next j
01220   fl4$(21)="1,1,C 80,H,N"
01230   fl4$(22)="2,1,C 80,H,N"
01240   fl4$(23)="13,2,c 78,n"
01250   for j=1 to 20
01260     fl5$(j)=str$(j+3)&",2,c 30,n"
01270     ot5$(j)=str$(j+3)&",35,n 8.2,ut,n"
01280     in5$(j)=str$(j+3)&",35,n 8.2,ut,n"
01290     ot5$(j+20)=str$(j+3)&",60,n 8.2,ut,n"
01300     in5$(j+20)=str$(j+3)&",60,n 8.2,ut,n"
01310     fl6$(j)=str$(j+3)&",2,c 30,n"
01320     ot6$(j)=str$(j+3)&",35,n 10.2,ut,n"
01330     in6$(j)=str$(j+3)&",35,n 10.2,ut,n"
01340     ot6$(j+20)=str$(j+3)&",60,n 10.2,ut,n"
01350     in6$(j+20)=str$(j+3)&",60,n 10.2,ut,n"
01360   next j
01370   fl5$(21)="1,1,C 80,H,N"
01380   fl5$(22)="2,1,C 80,H,N"
01390   fl6$(21)="1,1,C 80,H,N"
01400   fl6$(22)="2,1,C 80,H,N"
01410   gosub L1720
01420 ! GOTO 1740 ! TEST SCREEN
01430   open #1: "Name=S:\acsTM\TMSCRN.CL",internal,input ioerr L1450
01440   close #1,free: 
01450 L1450: open #1: "Name=S:\acsTM\TMSCRN.CL,SIZE=0,RecL=3790",internal,output ioerr L2000
01460   write #1,using L1470: mat scr1$,mat fl1$,mat oi1$,mat scr2$,mat fl2$,mat ot2$,mat ot2$,mat flh$,mat flit$,mat desc$,mat scrt$
01470 L1470: form pos 1,31*c 20,71*c 18,14*c 25,79*c 18,6*c 20
01480   close #1: 
01490   open #1: "Name=S:\acsTM\TMSCRN.EM",internal,input ioerr L1510
01500   close #1,free: 
01510 L1510: open #1: "Name=S:\acsTM\TMSCRN.EM,SIZE=0,RecL=1854",internal,output ioerr L2000
01520   write #1,using L1530: mat fl3$,mat ot3$,mat in3$
01530 L1530: form pos 1,47*c 18
01540   write #1,using L1550: mat fl4$,mat ot4$,mat in4$
01550 L1550: form pos 1,103*c 18
01560   write #1,using L1570: mat fl5$,mat ot5$,mat in5$
01570 L1570: form pos 1,102*c 18
01580   write #1,using L1590: mat fl6$,mat ot6$,mat in6$
01590 L1590: form pos 1,102*c 18
01600   close #1: 
01610   chain "S:\acsTM\ARINPSCR"
01620   dim z$*5,a$(5)*30,ph$*12,ss$*11,dd(10),sc(10),ca(10),ar(5)
01630   dim ph2$*12,ss2$*11,cm$*70
01640   pr newpage
01650   scrid$(1)="  TIME MANAGEMENT KDKDKDKD"
01660   scrid$(2)=" FDAS;JFDASKJLFDSKJALFDASKJLFDSKJL"
01670   pr f mat fl1$: mat scr1$,mat scrid$
01680   input fields mat oi1$: clinum,mat a$,ph$,ss$,pno,mye,ph2$,ss2$,mat ar,cm$,mat dd,mat sc
01690   pause 
01700   dim flit$(6)*18,scrt$(6)*20,scrz$(2)*79,desc$(8)*18
01710   dim hlp$(20)*78,flh$(22)*18,hhd$*60
01720 L1720: flit$(1)="6,22,c 12,ut,N"
01730   flit$(2)="8,22,n 6,ut,N"
01740   flit$(3)="10,22,n 11.2,ut,N"
01750   flit$(4)="12,22,c 20,ut,N"
01760   flit$(5)="14,22,n 3,ut,N"
01770   flit$(6)="16,22,n 1,ut,N"
01780   desc$(1)="3,2,c 79,n"
01790   desc$(2)="4,2,c 79,n"
01800   desc$(3)="6,2,c 20,n"
01810   desc$(4)="8,2,c 20,n"
01820   desc$(5)="10,2,c 20,n"
01830   desc$(6)="12,2,c 20,n"
01840   desc$(7)="14,2,c 20,n"
01850   desc$(8)="16,2,c 20,n"
01860   scrt$(1)="INVOICE NUMBER"
01870   scrt$(2)="DATE"
01880   scrt$(3)="AMOUNT"
01890   scrt$(4)="INVOICE DESCRIPTION"
01900   scrt$(5)="SALESMAN NUMBER"
01910   scrt$(6)="TRANSACTION CODE"
01920 ! DIM, OPEN AND BLDSCR
01930   dim hlp$(20)*78,flh$(22)*18,hhd$*60
01940   for j=1 to 20
01950     flh$(j)=str$(j+2)&",2,C 78,ut,N"
01960   next j
01970   flh$(21)="1,25,C 40,H,N"
01980   flh$(22)="24,5,C 65,H,N"
01990   return 
02000 L2000: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2020
02010   goto L2060
02020 L2020: pr newpage
02030   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2050
02040   goto L2060
02050 L2050: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
02060 L2060: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
02070   input fields "24,60,C 1,N": quitcode$
02080   if rtrm$(uprc$(quitcode$))="Q" then goto L2120
02090   pr f "23,3,C 78,N": ""
02100   pr f "24,3,C 78,N": ""
02110   retry 
02120 L2120: chain "S:\Core\Menu"
