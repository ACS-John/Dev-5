00010 ! Replace S:\acsUB\ubprtbl1_chatom ! based on ubprtbl1_mow
00020 ! pr bills for Chatom
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnpa_txt,fnpa_finis,fnpa_open,fnpa_newpage
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
00100 ! ______________________________________________________________________
00110   fncno(cno,cnam$)
00120   fnd1(d1)
00130   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input 
00140   read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
00150   close #21: 
00160   cnam$='Chatom Utilities'
00170   let z=21
00180   at$(1)=trim$(at$(1))(1:z)
00190   let x=len(at$(1)) : let y=z-x
00200   at$(1)=rpt$(" ",int(y/2))&at$(1)
00210   let z=26
00220   for j=2 to udim(at$)
00230     at$(j)=trim$(at$(j))(1:z)
00240     let x=len(at$(j)) : let y=z-x
00250     at$(j)=rpt$(" ",int(y/2))&at$(j)
00260   next j
00270   let linelength=62
00280 ! 
00290 ! 
00300   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
00310   fn_bulksort
00320   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.H"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00330   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.H"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00340 ! ______________________________________________________________________
00350 SCREEN1: ! 
00360   a$="" : let prtbkno=0
00370   fntos(sn$="UBPrtBl1-1")
00380   let pf=26 : let ll=24
00390   let respc=0
00400   fnlbl(3,1,"Penalty Due Date:",ll,1)
00410   fntxt(3,pf,8,8,1,"1",0,tt$)
00420   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00430   fnlbl(4,1,"Message on Bill:",ll,1)
00440   fntxt(4,pf,30,30)
00450   let resp$(respc+=1)=mg$(1)
00460   fntxt(5,pf,30,30)
00470   let resp$(respc+=1)=mg$(2)
00480   fntxt(6,pf,30,30)
00490   let resp$(respc+=1)=mg$(3)
00500   fnlbl(7,1,"Date of Billing:",ll,1)
00510   fntxt(7,pf,8,8,1,"1")
00520   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00530   fnlbl(8,1,"Starting Account:",ll,1)
00540   let fe$="ubm-act-nam"
00550   let datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno)
00560   let indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno)
00570   let kp=1741: let kl=9 : let dp=41 : let dl=30
00580   fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2)
00590   let resp$(respc+=1)="[All]"
00600   fnlbl(9,1,"Route Number:",ll,1)
00610   fncmbrt2(9,pf)
00620   let resp$(respc+=1)="[All]"
00630   fnchk(10,pf,"Select Accounts to Print",1)
00640   let resp$(respc+=1)="False"
00650   fncmdset(3)
00660   fnacs(sn$,0,mat resp$,ck)
00670   if ck=5 then goto ENDSCR
00680   let d1=val(resp$(5))
00690   let d4=val(resp$(1))
00700   let mg$(1)=resp$(2)
00710   let mg$(2)=resp$(3)
00720   let mg$(3)=resp$(4)
00730   if resp$(6)="[All]" then a$="" else a$=lpad$(trim$(resp$(6)(1:9)),9)
00740   if resp$(7)="[All]" then let prtbkno=0 else let prtbkno=val(resp$(7))
00750   if resp$(8)="True" then let sl1=1: let z$="" else let sl1=0
00760   if trim$(a$)<>"" then 
00770     read #2,using L460,key=a$: z$,route,sequence nokey SCREEN1
00780     let st1=1
00790     let st1$=z$
00800   end if 
00805 L460: form pos 1,c 10,pos 1741,n 2,n 7
00810   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00815   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00820   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00825 ! ______________________________________________________________________
00830   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.H"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.H"&str$(cno)&",Shr",internal,input,keyed ioerr FAIL_OPEN_3
00835 FAIL_OPEN_3: ! 
00840   fnPa_open("Landscape")
00845   let lyne=3
00850   character=1.5
00855 ! ______________________________________________________________________
00860   on fkey 5 goto RELEASE_PRINT
00862 L550: if sl1=1 then goto SCREEN3
00864 L560: read #6,using L570: z$ eof RELEASE_PRINT
00866 L570: form pos 22,c 10
00868   read #1,using L590,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,sequence nokey L560
00870 L590: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1743,n 7
00872   if prtbkno=0 then goto L620
00874   if prtbkno><route then goto RELEASE_PRINT
00876 L620: if f><d1 then goto L550
00878   if st1=0 then goto READALTADR
00880   if st1$=z$ then let st1=0 else goto L550
00882 READALTADR: ! 
00884 ! read alternate billing address
00886   read #3,using L680,key=z$: mat ba$ nokey L750 ioerr L750
00888 L680: form pos 11,4*c 30
00890   let e1=0 : mat pe$=("")
00892   for j=1 to 4
00894     if rtrm$(ba$(j))<>"" then let e1=e1+1 : let pe$(e1)=ba$(j)
00896   next j
00898   goto L900
00900 ! ______________________________________________________________________
00902 L750: let e1=0 : mat pe$=("")
00904   for j=2 to 4
00906     if rtrm$(e$(j))<>"" then let e1=e1+1 : let pe$(e1)=e$(j)
00908   next j
00910   if trim$(extra1$)<>"" then let pe$(4)=pe$(3): let pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00912   goto L900
00914 ! ______________________________________________________________________
00916 RELEASE_PRINT: ! 
00918   close #1: ioerr L840
00920 L840: close #3: ioerr L850
00922 L850: let fnpa_finis
00924   goto ENDSCR
00926 ! ______________________________________________________________________
00928 L900: ! 
00930   let pb=bal-g(11)
00932   if bal<=0 then let g(10)=0 ! don't show penalty if balance 0 or less
00934 ! ______________print bill routine______________________________________
00936   fn_vbprint
00938 ! _____________end of pr routine______________________________________
00940   bct(2)=bct(2)+1 ! accumulate totals
00942   goto L550
00944 ! ______________________________________________________________________
00946 SCREEN3: ! 
00948   let sn$="UBPrtBl1-2"
00950   fntos(sn$)
00952   let txt$="Account (blank to stop)"
00954   fnlbl(1,1,txt$,31,1)
00956 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
00958   if trim$(z$)<>"" then 
00960     let txt$="Last Account entered was "&z$
00962     fnlbl(3,1,txt$,44,1)
00964   else 
00966     let txt$=""
00968     fnlbl(3,1,txt$,44,1)
00970   end if 
00972   fncmbact(1,17) ! 
00974   let resp$(1)=a$
00976   fncmdset(3): let fnacs(sn$,0,mat resp$,ck)
00978   a$=lpad$(trim$(resp$(1)(1:10)),10)
00980   if trim$(a$)="" then goto RELEASE_PRINT
00982   if ck=5 then goto RELEASE_PRINT
00984   read #1,using L590,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,sequence nokey SCREEN3
00986   goto READALTADR
00988 ! ______________________________________________________________________
00990 SORT1: ! SELECT & SORT
00992   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1370
00994   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
00996   let s5=1
00998   if prtbkno=0 then let routekey$="" else let routekey$=cnvrt$("N 2",prtbkno)&"       " ! key off first record in route (route # no longer part of customer #)
01000   restore #2,search>=routekey$: 
01170 L1170: read #2,using L1180: z$,f,route eof END5
01180 L1180: form pos 1,c 10,pos 296,pd 4,pos 1741
01190   if prtbkno=0 then goto L1210
01200   if prtbkno><route then goto END5
01210 L1210: if f><d1 then goto L1170
01220   let zip5$=cr$=""
01230   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1240
01240 L1240: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
01250   goto L1170
01260 ! ______________________________________________________________________
01270 END5: close #6: 
01280   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01290 L1290: form pos 1,c 128
01300   write #9,using L1290: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01310   write #9,using L1290: "Mask 1,19,C,A"
01320   close #9: 
01330   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1340
01340 L1340: execute "Sort "&env$('Temp')&"\Control."&session$
01350   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01360   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01370 L1370: return 
01380 ! ______________________________________________________________________
01390 ENDSCR: ! pr totals screen
01400   if sum(bct)=0 then let pct=0 else let pct=bct(2)/sum(bct)*100
01410   fntos(sn$="Bills-Total")
01420   let mylen=23 : let mypos=mylen+2
01430   let respc=0
01440   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01450   fntxt(1,mypos,8,0,1,"",1)
01460   let resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01490 ! Let FNLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01500 ! Let FNTXT(2,MYPOS,8,0,1,"",1)
01510 ! Let RESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01520 ! Let FNLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01530 ! Let FNTXT(3,MYPOS,8,0,1,"",1)
01540 ! Let RESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01550 ! Let FNLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01560 ! Let FNTXT(4,MYPOS,8,0,1,"",1)
01570 ! Let RESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01580   fncmdset(52)
01590   fnacs(sn$,0,mat resp$,ck)
01600 XIT: let fnxit
01610 ! ______________________________________________________________________
01620 ERTN: let fnerror(program$,err,line,act$,"xit")
01630   if uprc$(act$)<>"PAUSE" then goto L1570
01640   execute "List -"&str$(line) : pause : goto L1570
01650   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01655 L1570: execute act$
01660   goto ERTN
01665 ! ______________________________________________________________________
01670   def fn_vbprint
01675 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
01680     checkcounter+=1
01685     if checkcounter=1 then let xmargin=0 : let ymargin=0
01690     if checkcounter=2 then let xmargin=140 : let ymargin=0
01695     if checkcounter=3 then let xmargin=0 : let ymargin=108
01700     if checkcounter=4 then let xmargin=140 : let ymargin=108 : checkcounter=0
01705 ! ______________________________________________________________________
01710     pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',55,'&str$(lyne*3+3)&',True)'
01715     pr #20: "Call Print.MyFontBold(True)"
01720     pr #20: 'Call Print.MyFontSize(10)'
01725     pr #20: 'Call Print.MyFont("Courier New")'
01730     fnpa_txt(cnam$,xmargin+16,lyne*1-1+ymargin)
01735     pr #20: 'Call Print.MyFont("Lucida Console")'
01740     pr #20: 'Call Print.MyFontSize(10)'
01745     pr #20: 'Call Print.MyFontBold(False)'
01750     fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2)
01755     fnpa_txt(at$(3),xmargin+6 ,lyne*3+1+ymargin)
01760     fnpa_txt("#"&trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
01765     fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
01770 ! let fnpa_txt("From: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3),xmargin+2,lyne*7+ymargin)
01775     pr #20: 'Call Print.AddText("Is due now and payable.",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
01780     pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
01785     pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
01790     pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
01795     pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
01800     pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
01805 ! ______________________________________________________________________
01810 PRINTGRID: let meter=14
01815     pr #20: 'Call Print.MyFontSize(8)'
01820     if g(1) then 
01825       pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
01830       pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
01835       pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
01840       pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
01845     end if 
01850     if g(2) then 
01855       pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
01860       pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
01865     end if 
01870     if g(3) then 
01875       pr #20: 'Call Print.AddText("EL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
01880       pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
01885       pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
01890       pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
01895     end if 
02020 L2020: ! 
02030     if a4=1 then 
02040       let gcode$="RSGS"
02050     else if a4=2 then 
02060       let gcode$="CMGS"
02070     else if a4=3 then 
02080       let gcode$="INGS"
02090     else 
02100       let gcode$="GAS"
02110     end if 
02120     if g(4) then 
02130       fnpa_txt(gcode$,xmargin+1,lyne*(meter+=1)+ymargin)
02140       pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
02150       pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
02160       pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02170     end if 
02180     if g(5) then 
02190       pr #20: 'Call Print.AddText("SAN",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02200       pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02210     end if 
02220     if g(6) then 
02230       pr #20: 'Call Print.AddText("FP",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02240       pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02250     end if 
02260     if g(7) then 
02270       pr #20: 'Call Print.AddText("FUEL ADJ",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02280       pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02290     end if 
02300     if g(8) then 
02310       pr #20: 'Call Print.AddText("Misc",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02320       pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02330     end if 
02340     if g(9) then 
02350       pr #20: 'Call Print.AddText("Tax",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02360       pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02370     end if 
02380     if pb then 
02390       pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02400       pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02410     end if 
02420     pr #20: 'Call Print.MyFontSize(10)'
02430 ! ______________________________________________________________________
02440     pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin)&',63,0)'
02450     pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin)&')'
02460     fnpa_txt(fnformnumb$(bal,2,9),xmargin+40,lyne*24+ymargin)
02470     pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin)&')'
02480     fnpa_txt(fnformnumb$(bal+g(10),2,9),xmargin+40,lyne*25+ymargin)
02490     pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin)&',63,0)'
02500     fnpa_txt("Phone: 251-847-2858",xmargin+1,lyne*27+ymargin)
02502     pr #20: 'Call Print.MyFontSize(8)'
02504     fnpa_txt("Service will be discontinued after ",xmargin+1,lyne*28+ymargin)
02506     fnpa_txt("due date without further notice.",xmargin+1,lyne*29+ymargin)
02508     pr #20: 'Call Print.MyFontSize(10)'
02510 ! ______________________________________________________________________
02520     let special=28
02530 ! ______________________________________________________________________
02540 ! pr #20: 'Call Print.MyFontSize(7)'
02550 ! pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
02560 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
02570 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
02580 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
02590 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
02600 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
02610 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
02620 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
02630 ! pr #20: 'Call Print.AddText("   Pre-Sorted",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
02640 ! pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
02650 ! pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
02660 ! pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
02670 ! pr #20: 'Call Print.AddText("  Permit No 38",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
02680     pr #20: 'Call Print.MyFontSize(9)'
02690 ! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
02700     fnpa_txt("Please return this",xmargin+68,lyne*11+ymargin)
02710     fnpa_txt("side with payment to:",xmargin+68,lyne*12+ymargin)
02720     fnpa_txt(cnam$,xmargin+68,lyne*13+ymargin)
02730     pr #20: 'Call Print.MyFontSize(10)'
02740     fnpa_txt("Pay By "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&":",xmargin+68,lyne*25+ymargin)
02750     fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*25+ymargin)
02760     fnpa_txt('After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*29+ymargin)
02770     fnpa_txt(fnformnumb$(bal+g(10),2,9),xmargin+106,lyne*29+ymargin)
02780     pr #20: 'Call Print.MyFontSize(9)'
02790     addy=14
02800     fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
02810     fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
02820     fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
02830     addy+=1
02840     pr #20: 'Call Print.MyFontSize(10)'
02850     if df$="Y" then 
02860       pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02870     end if 
02880     if c4>0 then 
02890       pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02900     end if 
02910     if d(10)=1 then 
02920       pr #20: 'Call Print.AddText("Bill Estimated",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02930     end if 
02940     fnpa_txt("#"&trim$(z$)&' '&bulk$,xmargin+68,lyne*(addy+=1)+ymargin)
02950     fnpa_txt(pe$(1),xmargin+68,lyne*(addy+=1)+ymargin)
02960     fnpa_txt(pe$(2),xmargin+68,lyne*(addy+=1)+ymargin)
02970     fnpa_txt(pe$(3),xmargin+68,lyne*(addy+=1)+ymargin)
02980     fnpa_txt(pe$(4),xmargin+68,lyne*(addy+=1)+ymargin)
02990     if checkcounter=1 then checkx=1.375 : checky=3.6875
03000     if checkcounter=2 then checkx=6.75 : checky=3.6875
03010     if checkcounter=3 then checkx=1.375 : checky=7.9375
03020     if checkcounter=0 then checkx=6.75 : checky=7.9375
03050     if checkcounter=0 then let fnpa_newpage
03060   fnend 
03070 ! ______________________________________________________________________
03080   def fn_bulksort ! bulk sort order
03090     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
03100     open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
03105 L2730: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2760
03110     write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
03115     goto L2730
03117 L2760: close #1: ioerr ignore
03119     close #6: ioerr ignore
03121     execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\Tempidx."&wsid$&" 1,19,Replace,DupKeys -n" ! ioerr L2800
03123     open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\Tempidx."&wsid$,internal,input,keyed 
03125 ! L2800: !
03127   fnend 
