00020 ! pr utility billing reports based on bills
00030   on fkey 5 goto DONE
00040   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fntos,fndat,fnerror,fnopenprn,fncloseprn,fncno,fnxit,fndate_mmddyy_to_ccyymmdd,fnd1,fncmdset,fntop,fnpause,fnchk,fncmbact,fnopt,fnd1
00050   dim cap$*128,sendto$*80,z$*10,e2$*30,temp$(3)*26,resp$(10)*50,cnam$*40
00060   let fntop("S:\Utility Billing\Billing Journal",cap$="Secondary Water Usage Report")
00065   let fnd1(d1)
00070   let fncno(cno,cnam$)
00090 ! ______________________________________________________________________
00100   let fntos(sn$="billingrpt")
00110   let fnlbl(1,1,"First Account:",25,1)
00120   let fncmbact(1,28) !:
        let resp$(1)=selz$
00130   let fnlbl(2,1,"Last Account:",25,1)
00140   let fncmbact(2,28) !:
        let resp$(2)=selz$
00150   let fnchk(3,29,"Print Grand Totals:",1)
00160   let resp$(3)="True"
00170   let fnchk(4,29,"Print Details:",1)
00180   let resp$(4)="True"
00190   let fnlbl(5,1,"Billing Date:",25,1)
00200   let fntxt(5,28,8,0,right,"1001",0,"Enter the last billing date.",0 ) !:
        let resp$(5)=str$(d1)
00210   let fncmdset(2)
00220   let fnacs(sn$,0,mat resp$,ckey)
00230   if ckey=5 then goto XIT
00240   let fan$=lpad$(rtrm$(resp$(1)(1:10)),10)
00250   let lan$=lpad$(rtrm$(resp$(2)(1:10)),10)
00260   if resp$(3)="True" then let print_grand_totals$="Y"
00270   if resp$(4)="True" then let print_details$="Y"
00280   let d1=val(resp$(5))
00290   goto STARTREPORT
00300 ! ______________________________________________________________________
00310 DONE: ! 
00320   let fncloseprn
00330 XIT: let fnxit
00340 ! ______________________________________________________________________
00350 STARTREPORT: ! 
00370   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00380 L380: form pos 1,c 10,pos 41,c 30,pos 227,pd 5,pos 296,pd 4
00390   let fnopenprn
00400 ! sort prep !!!
00410   open #6: "Name="&env$('temp')&"\Work."&session$&",REPLACE,RecL=50",internal,output 
00420   if fan$>"" then restore #1,search>=fan$: 
00430 L430: read #1,using L380: z$,e2$,d3,f eof SORT_NOW
00440   if f<>d1 then goto L430
00450   if trim$(lan$)<>"" and z$>lan$ then goto SORT_NOW
00460   write #6,using L470: d3,z$,e2$
00470 L470: form pos 1,n 10,c 10,c 30
00480   goto L430
00490 SORT_NOW: ! 
00500   close #1: : close #6: 
00510   open #9: "Name="&env$('Temp')&"\Control."&session$&",SIZE=0,RecL=128,REPLACE",internal,output 
00520 L520: form pos 1,c 128
00530   write #9,using L520: "FILE "&env$('temp')&"\Work."&session$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
00540   write #9,using L520: "MASK 1,10,N,A,11,10,C,A"
00550   close #9: 
00560   execute "FREE "&env$('Temp')&"\Addr."&session$&" -n" ioerr L570
00570 L570: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
00580   open #6: "Name="&env$('temp')&"\Work."&session$,internal,input,relative 
00590   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
00600   gosub HEADER_PAGE
00610   goto REPORT
00620 ! ______________________________________________________________________
00630 HEADER_PAGE: ! 
00640   let pg=pg+1
00650   if print_details$="N" then let temp$(1)="No Details"
00660   if print_grand_totals$="N" then let temp$(3)="No Usage Totals"
00670 L670: form pos 1,c 20,x 5,cc 40,cr 20,skip 1
00680 L680: form pos 1,cc 30,cc 30,cc 30,skip 1
00690 L690: form pos 1,cc 10,x 1,cc 30,x 1,cc 10
00700   pr #255: ""
00710   pr #255,using L670: "Company Number "&str$(cno),cnam$,"Page "&str$(pg)
00720   pr #255,using L680: temp$(1),cap$,temp$(3)
00730   pr #255,using L740: "Billing Date: "&cnvrt$("pic(zz/zz/zz)",d1)
00740 L740: form pos 30,cc 30,skip 1
00750   pr #255: ""
00760   gosub HEADER_COLUMN
00770   return 
00780 ! ______________________________________________________________________
00790 HEADER_COLUMN: ! 
00800   if print_details$="N" then goto L830
00810   pr #255,using L690: "Act.Number","Customer Name","WaterUsage"
00820   pr #255,using L690: "__________","______________________________","__________"
00830 L830: return 
00840 ! ______________________________________________________________________
00850 PGOF: ! 
00860   pr #255: newpage
00870   if no_more_header=0 then gosub HEADER_PAGE
00880   let no_more_header=0
00890   continue 
00900 ! ______________________________________________________________________
00910 REPORT: ! 
00920   read #7,using L930: r6 eof GRANDTOTAL
00930 L930: form pos 1,pd 3
00940   read #6,using L470,rec=r6: d3,z$,e2$ eof GRANDTOTAL
00950   if subtotal_break_check<>d3 and not_first_rec=1 then gosub SUBTOTAL
00960   let subtotal_count+=1
00970   let subtotal_d3+=d3
00980   if print_details$="Y" then gosub DETAILS
00990   let subtotal_break_check=d3 ! vaL(Z$(1:2))
01000   let not_first_rec=1
01010   goto REPORT
01020 ! ______________________________________________________________________
01030 DETAILS: ! 
01040   pr #255,using L1050: z$,e2$,d3 pageoflow PGOF
01050 L1050: form pos 1,c 10,x 1,c 30,x 1,n 10
01060   return 
01070 ! ______________________________________________________________________
01080   return 
01090 ! ______________________________________________________________________
01100 SUBTOTAL: ! 
01110   if print_details$="N" then pr #255,using L1120: "SubTotals (for usage of "&str$(subtotal_break_check)&")","Customer Count: "&str$(subtotal_count),"Water Usage: "&str$(subtotal_d3) pageoflow PGOF: goto L1180
01120 L1120: form pos 1,c 32,x 3,c 22,x 3,c 26,skip 1
01130   pr #255,using L690: "__________","______________________________","__________" pageoflow PGOF
01140   pr #255,using L1150: "SubTotals (for usage of "&str$(subtotal_break_check)&")" pageoflow PGOF
01150 L1150: form pos 1,c 80
01160   pr #255,using L1150: "Customer Count: "&str$(subtotal_count) pageoflow PGOF
01170   pr #255,using L1150: "Water Usage: "&str$(subtotal_d3) pageoflow PGOF
01180 L1180: pr #255: "" pageoflow PGOF
01190   let grandtotal_count+=subtotal_count
01200   let grandtotal_d3+=subtotal_d3
01210   let subtotal_count=subtotal_d3=0
01220   if no_more_header=0 then gosub HEADER_COLUMN
01230   return 
01240 ! ______________________________________________________________________
01250 GRANDTOTAL: ! 
01260   let no_more_header=1
01270   gosub SUBTOTAL
01280   if print_grand_totals$="N" then goto L1340
01290   pr #255,using L1150: "____________________________________________________" pageoflow PGOF
01300   pr #255,using L1150: "Grand Totals"
01310   pr #255,using L1150: "Customer Count: "&str$(grandtotal_count) pageoflow PGOF
01320   pr #255,using L1150: "Water Usage: "&str$(grandtotal_d3) pageoflow PGOF
01330   pr #255,using L1150: "____________________________________________________" pageoflow PGOF
01340 L1340: goto DONE
01350 ! ______________________________________________________________________
