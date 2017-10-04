00020 ! average sewer usage program
00030   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fntos,fndat,fnerror,fnopenprn,fncloseprn,fncno,fnxit,fndate_mmddyy_to_ccyymmdd,fnd1,fncmdset,fntop,fnpause,fnchk,fncmbact,fnopt
00040   dim cap$*128,sendto$*80,z$*10,e$(4)*30,temp$(3)*26
00050   fntop("S:\Utility Billing\Billing Journal",cap$="Average Sewer Usage Report")
00060   fncno(cno,cnam$)
00080 ! ______________________________________________________________________
00090   on fkey 5 goto DONE
00100   fntos(sn$="sewerusage")
00110   fnlbl(1,5,"Show Customers who have sewer usage")
00120   fnlbl(2,1,"Less than:",20,1)
00130   fntxt(2,23,9,0,right,"30",0,"Enter the minimum usage you want cosidered in your report. (Blank for all)",0 ) !:
        let resp$(2)=""
00140   fnlbl(3,1,"Greater than:",20,1)
00150   fntxt(3,23,9,0,right,"30",0,"Enter the maximum usage you want cosidered in your report (Blank for all).",0 ) !:
        let resp$(3)=""
00160   fncmdset(2)
00170   fnacs(sn$,0,mat resp$,ckey)
00180   if ckey=5 then goto XIT
00190   let minu=val(resp$(1))
00200   let maxu=val(resp$(2))
00210   goto STARTREPORT
00220 ! ______________________________________________________________________
00230 DONE: ! 
00240   fncloseprn
00250 XIT: let fnxit
00260 ! ______________________________________________________________________
00270 STARTREPORT: ! 
00280 ! maybe a printing please wait screen here would be nice.
00290   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00300   fnopenprn
00310   gosub HEADER
00320   goto REPORT
00330 ! ______________________________________________________________________
00340 HEADER: ! 
00350   let pg=pg+1
00360 L360: form pos 1,c 20,cc 40,cr 20,skip 1
00370   form pos 1,cc 26,cc 26,cc 26,skip 1
00380 L380: form pos 1,cc 10,x 1,cc 30,x 1,cc 10,x 1,cc 17,x 1,cc 10,skip 1
00390   pr #255,using L360: "",cap$,""
00400   pr #255,using L360: "Company Number "&str$(cno),cnam$,"Page "&str$(pg)
00410   pr #255: ""
00420   pr #255: ""
00430   pr #255,using L380: "Act.Number","Customer Name","Balance","Last Billing Date","Average"
00440   pr #255,using L380: "__________","______________________________","__________","_________________","__________"
00450   return 
00460 ! ______________________________________________________________________
00470 PGOF: ! 
00480   pr #255: newpage
00490   gosub HEADER
00500   continue 
00510 ! ______________________________________________________________________
00520 REPORT: ! 
00530 L530: read #1,using L540: z$,mat e$,final,average,bal,lastbilldate eof DONE
00540 L540: form pos 1,c 10,4*c 30,pos 1821,n 1,pos 1822,n 9,pos 292,pd 4.2,pd 4
00550   if minu=0 and maxu=0 then goto L570
00560   if (minu>0 and average<minu) or (maxu>0 and average>maxu) then goto L530 else goto L570
00570 L570: pr #255,using L580: z$,e$(2),bal,lastbilldate,average pageoflow PGOF
00580 L580: form pos 1,c 10,x 1,c 30,x 1,n 10.2,x 1,pic(zz/zz/zz),x 8,n 10
00590   goto L530
00600 ! ______________________________________________________________________
