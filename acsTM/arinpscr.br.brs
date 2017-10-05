00020 ! 
00022   on error goto L1320
00030   dim p$*5,iv$*12,tr(6),id$*20,sc1$(5),sc2$(9),fli1$(49),hd$(2)*50,ot1$(49)
00080   dim fl1$(7),flo1$(11),pgl(3),gl(10,4),sc3$(5),pt(5),flo3$(6)
00090   dim fli2$(49),ot2$(49),fli3$(49),ot3$(49),fli4$(49),ot4$(49)
00100   dim f1$*255,f2$*255,f3$*255,f4$*255
00110   f1$="FORM POS 1,C 5,C 12,N 6,2*PD 5.2,PD 2,2*N 1,C 20,n 3,n 6,n 3"
00120   f2$="FORM POS 1,C 5,C 12,N 6,2*PD 5.2,PD 2,2*N 1,C 20,x 3,n 6,n 3"
00130   f3$="FORM POS 1,C 5,C 12,N 6,2*PD 5.2,PD 2,2*N 1,C 20,n 3,n 6,x 3"
00140   f4$="FORM POS 1,C 5,C 12,N 6,2*PD 5.2,PD 2,2*N 1,C 20,x 3,n 6,x 3"
00150   for j=1 to 10
00160     f1$=rtrm$(f1$)&",n 3,n 6,n 3,pd 5.2"
00170     f2$=rtrm$(f2$)&",x 3,n 6,n 3,pd 5.2"
00180     f3$=rtrm$(f3$)&",n 3,n 6,x 3,pd 5.2"
00190     f4$=rtrm$(f4$)&",x 3,n 6,x 3,pd 5.2"
00200   next j
00210   open #1: "Name=S:\acsTM\TMSCRN.CL",internal,output ioerr L1320
00220   data "0 = COMPLETED"
00230   data "1 = INVOICES"
00240   data "2 = DEBIT MEMOS"
00250   data "3 = COLLECTIONS"
00260   data "4 = CREDIT MEMOS"
00270   read mat sc1$
00280   data "CLIENT #"
00290   data "INVOICE #"
00300   data "DATE"
00310   data "AMOUNT"
00320   data "DESCRIPTION"
00330   data "COST OF GOODS"
00340   data " "
00350   data " G/L ACCOUNT #"
00360   data "    AMOUNT"
00370   read mat sc2$
00380   sc3$(1)="TOTAL ACCOUNT #'S"
00390   for j=2 to 5
00400     sc3$(j)="TOTAL "&sc1$(j)(5:17)
00410   next j
00420   for j=1 to 6
00430     flo3$(j)=str$(j+5)&",26,n 11.2"
00440   next j
00450   for j=1 to 6
00460     flo1$(j)=str$(j+2)&",5,c 20"
00470     if j>5 then goto L490
00480     fl1$(j)=str$(j+5)&",5,c 20"
00490 L490: next j
00500   fl1$(6)="3,10,c 50,h,n"
00510   fl1$(7)="13,10,c 50,h,n"
00520   flo1$(7)="9,5,c 20"
00530   flo1$(8)="11,20,c 20"
00540   flo1$(9)="11,40,c 20"
00550   flo1$(10)="1,15,c 40,h,n"
00560   flo1$(11)="2,5,c 45,h,n"
00570   fli1$(1)="3,30,C 5,ut,n"
00580   ot1$(1)="3,30,C 5,ut,n"
00590   fli1$(2)="4,30,c 12,cu,n"
00600   ot1$(2)="4,30,c 12,ut,n"
00610   fli1$(3)="5,30,n 6,ut,n"
00620   ot1$(3)="5,30,n 6,ut,n"
00630   fli1$(4)="6,30,n 11.2,ut,n"
00640   ot1$(4)="6,30,n 11.2,ut,n"
00650   fli1$(5)="7,30,c 20,ut,n"
00660   ot1$(5)="7,30,c 20,ut,n"
00670   fli1$(6)="8,30,n 11.2,ut,n"
00680   ot1$(6)="8,30,n 11.2,ut,n"
00690   fli1$(7)="9,30,n 3,ut,n"
00700   ot1$(7)="9,30,n 3,ut,n"
00710   fli1$(8)="9,34,n 6,ut,n"
00720   ot1$(8)="9,34,n 6,ut,n"
00730   fli1$(9)="9,41,n 3,ut,n"
00740   ot1$(9)="9,41,n 3,ut,n"
00750   for j=1 to 10
00760     fli1$(j*4+6)=str$(j+11)&",20,n 3,ut,n"
00770     ot1$(j*4+6)=str$(j+11)&",20,n 3,ut,n"
00780     fli1$(j*4+7)=str$(j+11)&",24,n 6,ut,n"
00790     ot1$(j*4+7)=str$(j+11)&",24,n 6,ut,n"
00800     fli1$(j*4+8)=str$(j+11)&",31,n 3,ut,n"
00810     ot1$(j*4+8)=str$(j+11)&",31,n 3,ut,n"
00820     fli1$(j*4+9)=str$(j+11)&",40,n 11.2,ut,n"
00830     ot1$(j*4+9)=str$(j+11)&",40,n 11.2,ut,n"
00840   next j
00850 L850: form pos 1,c 255,142*c 18
00860   for j=1 to 6
00870     fli2$(j)=fli1$(j)
00880     ot2$(j)=ot1$(j)
00890     fli3$(j)=fli1$(j)
00900     ot3$(j)=ot1$(j)
00910     fli4$(j)=fli1$(j)
00920     ot4$(j)=ot1$(j)
00930   next j
00940   fli2$(7)=fli1$(8)
00950   ot2$(7)=ot1$(8)
00960   fli4$(7)=fli1$(8)
00970   ot4$(7)=ot1$(8)
00980   fli3$(7)=fli1$(7)
00990   ot3$(7)=ot1$(7)
01000   fli2$(8)=fli1$(9)
01010   ot2$(8)=ot1$(9)
01020   fli3$(8)=ot1$(8)
01030   ot3$(8)=ot1$(8)
01040   for j=1 to 10
01050     fli2$(j*3+6)=fli1$(j*4+7)
01060     ot2$(j*3+6)=ot1$(j*4+7)
01070     fli2$(j*3+7)=fli1$(j*4+8)
01080     ot2$(j*3+7)=ot1$(j*4+8)
01090     fli2$(j*3+8)=fli1$(j*4+9)
01100     ot2$(j*3+8)=ot1$(j*4+9)
01110     fli3$(j*3+6)=fli1$(j*4+6)
01120     ot3$(j*3+6)=ot1$(j*4+6)
01130     fli3$(j*3+7)=fli1$(j*4+7)
01140     ot3$(j*3+7)=ot1$(j*4+7)
01150     fli3$(j*3+8)=fli1$(j*4+9)
01160     ot3$(j*3+8)=ot1$(j*4+9)
01170     fli4$(j*2+6)=fli1$(j*4+7)
01180     ot4$(j*2+6)=fli1$(j*4+7)
01190     fli4$(j*2+7)=fli1$(j*4+9)
01200     ot4$(j*2+7)=ot1$(j*4+9)
01210   next j
01220   write #1,using L850: f1$,mat fl1$,mat sc1$,mat sc2$,mat fli1$,mat ot1$,mat flo1$,mat flo3$,mat sc3$
01230   write #1,using L850: f2$,mat fl1$,mat sc1$,mat sc2$,mat fli2$,mat ot2$,mat flo1$,mat flo3$,mat sc3$
01240   write #1,using L850: f3$,mat fl1$,mat sc1$,mat sc2$,mat fli3$,mat ot3$,mat flo1$,mat flo3$,mat sc3$
01250   write #1,using L850: f4$,mat fl1$,mat sc1$,mat sc2$,mat fli4$,mat ot4$,mat flo1$,mat flo3$,mat sc3$
01260   sc2$(8)=" "
01270   sc2$(9)=" "
01280   fli1$(2)(11:11)=""
01290   write #1,using L850: f1$,mat fl1$,mat sc1$,mat sc2$,mat fli1$,mat ot1$,mat flo1$,mat flo3$,mat sc3$
01300   close #1: 
01310   chain "S:\acsTM\TMCOFM"
01320 L1320: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L1340
01330   goto L1380
01340 L1340: pr newpage
01350   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L1370
01360   goto L1380
01370 L1370: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
01380 L1380: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
01390   input fields "24,60,C 1,N": quitcode$
01400   if rtrm$(uprc$(quitcode$))="Q" then goto L1440
01410   pr f "23,3,C 78,N": ""
01420   pr f "24,3,C 78,N": ""
01430   retry 
01440 L1440: chain "S:\Core\Menu"
