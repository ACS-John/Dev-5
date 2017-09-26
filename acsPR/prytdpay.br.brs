00010 ! Replace S:\acsPR\prYTDPay
00020 ! PR Year To Date Pay Report
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnopenprn,fncloseprn,fndat
00050   let fntop("S:\acsPR\prytdpay",cap$="YTD Wage Breakdown")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cnam$*40,dat$*20,em1$*30,tdet(17),tdy(6),tdc(6),ty(21)
00090 ! ______________________________________________________________________
00100   let fncno(cno,cnam$) !:
        let fndat(dat$)
00110   let bob1=(132-len(rtrm$(cnam$)))/2 !:
        let bob2=(132-len(rtrm$(dat$)))/2 !:
        let bob3=132-10 : let bob4=(132-26)/2
00120   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&",Shr",internal,input,keyed 
00130   open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&str$(cno)&",Shr",internal,input,relative 
00140   print "please wait..."
00150   let fnopenprn(cp,0,0,process)
00160   gosub HDR
00170   goto LYNES
00180 ! ______________________________________________________________________
00190 NPG: print #255: newpage
00192   gosub HDR
00193   continue 
00200 HDR: ! 
00210   print #255,using L240: "PR Year To Date Pay Report","Page",pagenum+=1,cnam$,dat$
00220   print #255,using L250: "Emp-Numb","Name                          ","     Reg Wages","   Overtime","  Other Cmp","     Vacation","         Sick","      Holiday","      Total"
00230   print #255,using L250: "________","______________________________","______________","___________","___________","_____________","_____________","_____________","___________"
00240 L240: form pos bob4,c 40,pos bob3,c 6,n 3,skip 1,pos bob1,c 40,skip 1,pos bob2,c 40,skip 2
00250 L250: form pos 1,c 8,x 1,c 30,x 1,c 14,x 1,c 11,x 1,c 11,x 1,c 13,x 1,c 13,x 1,c 13,x 1,c 11,skip 1
00260 L260: form pos 1,n 8,x 1,c 30,x 1,n 14.2,x 1,n 11.2,x 1,n 11.2,x 1,n 13.2,x 1,n 13.2,x 1,n 13.2,x 1,n 11.2,skip 1
00270   return 
00280 LYNES: ! 
00290   let em1$=""
00300   let c3=0
00310   let c4=0
00320   let c5=0
00330   let c6=0
00340   let c7=0
00350   let c8=0
00360   let c9=0
00370 L370: read #2,using L380: teno,tli,mat tdet,mat tdy,mat tdc,mat ty,nta eof SUMMARY
00380 L380: form pos 1,n 8,pos 54,24*pd 4.2,6*pd 3.2,21*pd 5.2,pos 468,pd 3
00390 L390: form pos 9,c 30
00400 CALC: ! 
00410   let c4=ty(17)+c4
00420   let c5=ty(18)+c5
00430   let c6=tdet(2)*tdy(4)+c6
00440   let c7=tdy(3)*tdet(2)+c7
00450   let c8=tdy(5)*tdet(2)+c8
00460   let c9=ty(21)+c9
00470   let c3=ty(21)-c6-c7-c8+c3
00480   if nta<>0 then goto L370
00490   read #1,using L390,key=lpad$(str$(teno),8): em1$ nokey LYNES
00500 ! calc total(s)
00510   let tc3=tc3+c3
00520   let tc4=tc4+c4
00530   let tc5=tc5+c5
00540   let tc6=tc6+c6
00550   let tc7=tc7+c7
00560   let tc8=tc8+c8
00570   let tc9=tc9+c9
00580   print #255,using L260: teno,em1$,round(c3,2),round(c4,2),round(c5,2),round(c6,2),round(c7,2),round(c8,2),round(c9,2) pageoflow NPG
00590   goto LYNES
00600 ! ______________________________________________________________________
00610 SUMMARY: ! 
00620   print #255: ""
00630   print #255,using L710: "Total Regular Wages:",tc3
00640   print #255,using L710: "      Overtime Wages:",tc4
00650   print #255,using L710: "      Other Compensation:",tc5
00660   print #255,using L710: "      Vacation Wages:",tc6
00670   print #255,using L710: "      Sick Wages:",tc7
00680   print #255,using L710: "      Holiday Wages:",tc8
00690   print #255: ""
00700   print #255,using L710: "      Total Wages:",tc9
00710 L710: form pos 10,c 25,pos 37,n 19.2
00720 DONE: ! 
00730   let fncloseprn
00740 XIT: let fnxit
00750 ! ______________________________________________________________________
00760 ! <Updateable Region: ERTN>
00770 ERTN: let fnerror(program$,err,line,act$,"xit")
00780   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00790   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00800   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00810 ERTN_EXEC_ACT: execute act$ : goto ERTN
00820 ! /region
00830 ! ______________________________________________________________________
