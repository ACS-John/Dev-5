00010 ! Replace S:\acsPR\jcBldScr
00020 ! builds the jcScrn file and then chains to S:\acsPR\rpNames
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim fl1$(9),io1$(9),fl2$(15),io2$(15),sc3$(12),fl3$(13),io3$(12)
00080 ! ______________________________________________________________________
00090   fncno(cno)
00100 ! 
00110 ! ______________________________________________________________________
00120   for j=1 to 9
00130     fl1$(j)=str$(j+2)&",2,cr 20,n"
00140     if j<=1 then io1$(1)=str$(j+2)&",23,C 6,UT,N" : goto L240
00150     if j>2 then goto L170
00160     io1$(j)=str$(j+2)&",23,C 40,CUT,N" : goto L240
00170 L170: if j>5 then goto L190
00180     io1$(j)=str$(j+2)&",23,C 30,UT,N" : goto L240
00190 L190: if j>6 then goto L210
00200     io1$(j)=str$(j+2)&",23,N 6,UT,N" : goto L240
00210 L210: if j>8 then goto L230
00220     io1$(j)=str$(j+2)&",23,N 14.2,UT,N" : goto L240
00230 L230: io1$(j)=str$(j+2)&",23,N 2,UT,N"
00240 L240: next j
00250   for j=1 to 15
00260     fl2$(j)=str$(j+2)&",2,Cr 20,N"
00270     if j>1 then goto L290
00280     io2$(j)=str$(j+2)&",23,N 5,UT,N" : goto L340
00290 L290: if j>2 then goto L310
00300     io2$(j)=str$(j+2)&",23,C 25,UT,N" : goto L340
00310 L310: if j>13 then goto L330
00320     io2$(j)=str$(j+2)&",23,N 14.2,UT,N" : goto L340
00330 L330: io2$(j)=str$(j+2)&",23,N 3,UT,N"
00340 L340: next j
00350   for j=1 to 12 : fl3$(j)=str$(j+3)&",2,cr 30" : next j
00360   fl3$(13)="2,5,c 70,h,n"
00370   data "Ref/Emp #:"
00380   data "Job Number:"
00390   data "Category:"
00400   data "Sub-Category:"
00410   data "P/R Dept #:"
00420   data "Date:"
00430   data "Reg Hours:"
00440   data "Ovt Hours:"
00450   data "Units:"
00460   data "Payroll Tax:"
00470   data "Amount:"
00480   data "Description:"
00490   read mat sc3$
00500   io3$(1)="4,34,C 12,UT,N"
00510   io3$(2)="5,34,C 6,UT,N"
00520   io3$(3)="6,34,N 5,UT,N"
00530   io3$(4)="7,34,N 2,UT,N"
00540   io3$(5)="8,34,N 3,UT,N"
00550   io3$(6)="9,34,N 6,UT,N"
00560   for j=7 to 10
00570     io3$(j)=str$(j+3)&",34,N 8.2,UT,N"
00580   next j
00590   io3$(11)="14,34,N 10.2,UT,N"
00600   io3$(12)="15,34,C 30,UT,N"
00610   open #1: "Name=[Q]\PRmstr\JCSCRN.H[cno]",internal,input ioerr L630
00620   close #1,free: 
00630 L630: open #1: "Name=[Q]\PRmstr\JCSCRN.H[cno],Size=0,RecL=1530",internal,output 
00640   write #1,using L650: mat fl1$,mat io1$,mat fl2$,mat io2$,mat sc3$,mat fl3$,mat io3$
00650 L650: form pos 1,85*c 18
00660   close #1: 
00670   chain "S:\acsPR\rpNames"
00680 ! ______________________________________________________________________
00690 ! <Updateable Region: ERTN>
00700 ERTN: fnerror(program$,err,line,act$,"xit")
00710   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00720   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00730   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00740 ERTN_EXEC_ACT: execute act$ : goto ERTN
00750 ! /region
00760 ! ______________________________________________________________________
00770 XIT: chain "Menu"
