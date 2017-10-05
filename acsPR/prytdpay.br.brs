00010 ! Replace S:\acsPR\prYTDPay
00020 ! PR Year To Date Pay Report
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnopenprn,fncloseprn,fndat
00050   fntop("S:\acsPR\prytdpay",cap$="YTD Wage Breakdown")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cnam$*40,dat$*20,em1$*30,tdet(17),tdy(6),tdc(6),ty(21)
00090 ! ______________________________________________________________________
00100   fncno(cno,cnam$) !:
        fndat(dat$)
00110   bob1=(132-len(rtrm$(cnam$)))/2 !:
        bob2=(132-len(rtrm$(dat$)))/2 !:
        bob3=132-10 : bob4=(132-26)/2
00120   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&",Shr",internal,input,keyed 
00130   open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&str$(cno)&",Shr",internal,input,relative 
00140   pr "please wait..."
00150   fnopenprn(cp,0,0,process)
00160   gosub HDR
00170   goto LYNES
00180 ! ______________________________________________________________________
00190 NPG: pr #255: newpage
00192   gosub HDR
00193   continue 
00200 HDR: ! 
00210   pr #255,using L240: "PR Year To Date Pay Report","Page",pagenum+=1,cnam$,dat$
00220   pr #255,using L250: "Emp-Numb","Name                          ","     Reg Wages","   Overtime","  Other Cmp","     Vacation","         Sick","      Holiday","      Total"
00230   pr #255,using L250: "________","______________________________","______________","___________","___________","_____________","_____________","_____________","___________"
00240 L240: form pos bob4,c 40,pos bob3,c 6,n 3,skip 1,pos bob1,c 40,skip 1,pos bob2,c 40,skip 2
00250 L250: form pos 1,c 8,x 1,c 30,x 1,c 14,x 1,c 11,x 1,c 11,x 1,c 13,x 1,c 13,x 1,c 13,x 1,c 11,skip 1
00260 L260: form pos 1,n 8,x 1,c 30,x 1,n 14.2,x 1,n 11.2,x 1,n 11.2,x 1,n 13.2,x 1,n 13.2,x 1,n 13.2,x 1,n 11.2,skip 1
00270   return 
00280 LYNES: ! 
00290   em1$=""
00300   c3=0
00310   c4=0
00320   c5=0
00330   c6=0
00340   c7=0
00350   c8=0
00360   c9=0
00370 L370: read #2,using L380: teno,tli,mat tdet,mat tdy,mat tdc,mat ty,nta eof SUMMARY
00380 L380: form pos 1,n 8,pos 54,24*pd 4.2,6*pd 3.2,21*pd 5.2,pos 468,pd 3
00390 L390: form pos 9,c 30
00400 CALC: ! 
00410   c4=ty(17)+c4
00420   c5=ty(18)+c5
00430   c6=tdet(2)*tdy(4)+c6
00440   c7=tdy(3)*tdet(2)+c7
00450   c8=tdy(5)*tdet(2)+c8
00460   c9=ty(21)+c9
00470   c3=ty(21)-c6-c7-c8+c3
00480   if nta<>0 then goto L370
00490   read #1,using L390,key=lpad$(str$(teno),8): em1$ nokey LYNES
00500 ! calc total(s)
00510   tc3=tc3+c3
00520   tc4=tc4+c4
00530   tc5=tc5+c5
00540   tc6=tc6+c6
00550   tc7=tc7+c7
00560   tc8=tc8+c8
00570   tc9=tc9+c9
00580   pr #255,using L260: teno,em1$,round(c3,2),round(c4,2),round(c5,2),round(c6,2),round(c7,2),round(c8,2),round(c9,2) pageoflow NPG
00590   goto LYNES
00600 ! ______________________________________________________________________
00610 SUMMARY: ! 
00620   pr #255: ""
00630   pr #255,using L710: "Total Regular Wages:",tc3
00640   pr #255,using L710: "      Overtime Wages:",tc4
00650   pr #255,using L710: "      Other Compensation:",tc5
00660   pr #255,using L710: "      Vacation Wages:",tc6
00670   pr #255,using L710: "      Sick Wages:",tc7
00680   pr #255,using L710: "      Holiday Wages:",tc8
00690   pr #255: ""
00700   pr #255,using L710: "      Total Wages:",tc9
00710 L710: form pos 10,c 25,pos 37,n 19.2
00720 DONE: ! 
00730   fncloseprn
00740 XIT: fnxit
00750 ! ______________________________________________________________________
00760 ! <Updateable Region: ERTN>
00770 ERTN: fnerror(program$,err,line,act$,"xit")
00780   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00790   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00800   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00810 ERTN_EXEC_ACT: execute act$ : goto ERTN
00820 ! /region
00830 ! ______________________________________________________________________
