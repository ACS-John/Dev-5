! pr utility billing reports based on bills
	on fkey 5 goto DONE
	autoLibrary
	dim cap$*128,sendto$*80,z$*10,e2$*30,temp$(3)*26,resp$(10)*50,cnam$*40
	fnTop("S:\Utility Billing\Billing Journal",cap$="Secondary Water Usage Report")
	fnLastBillingDate(d1)
	fncno(cno,cnam$)
 
	fnTos(sn$="billingrpt")
	fnLbl(1,1,"First Account:",25,1)
	fncmbact(1,28) : _
	resp$(1)=selz$
	fnLbl(2,1,"Last Account:",25,1)
	fncmbact(2,28) : _
	resp$(2)=selz$
	fnChk(3,29,"Print Grand Totals:",1)
	resp$(3)="True"
	fnChk(4,29,"Print Details:",1)
	resp$(4)="True"
	fnLbl(5,1,"Billing Date:",25,1)
	fnTxt(5,28,8,0,right,"1001",0,"Enter the last billing date.",0 ) : _
	resp$(5)=str$(d1)
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	fan$=lpad$(rtrm$(resp$(1)(1:10)),10)
	lan$=lpad$(rtrm$(resp$(2)(1:10)),10)
	if resp$(3)="True" then print_grand_totals$="Y"
	if resp$(4)="True" then print_details$="Y"
	d1=val(resp$(5))
	goto STARTREPORT
 
DONE: !
	fncloseprn
Xit: fnXit
 
STARTREPORT: !
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
L380: form pos 1,c 10,pos 41,c 30,pos 227,pd 5,pos 296,pd 4
	fnopenprn
! sort prep !!!
	open #6: "Name=[Temp]\Work."&session$&",REPLACE,RecL=50",internal,output
	if fan$>"" then restore #1,search>=fan$:
L430: read #1,using L380: z$,e2$,d3,f eof SORT_NOW
	if f<>d1 then goto L430
	if trim$(lan$)<>"" and z$>lan$ then goto SORT_NOW
	write #6,using L470: d3,z$,e2$
L470: form pos 1,n 10,c 10,c 30
	goto L430
SORT_NOW: !
	close #1: : close #6:
	open #9: "Name=[Temp]\Control."&session$&",SIZE=0,RecL=128,REPLACE",internal,output
L520: form pos 1,c 128
	write #9,using L520: "FILE [Temp]\Work."&session$&",,,[Temp]\Addr."&session$&",,,,,A,N"
	write #9,using L520: "MASK 1,10,N,A,11,10,C,A"
	close #9:
	execute "FREE [Temp]\Addr."&session$&" -n" ioerr L570
L570: execute "Sort [Temp]\Control."&session$&" -n"
	open #6: "Name=[Temp]\Work."&session$,internal,input,relative
	open #7: "Name=[Temp]\Addr."&session$,internal,input,relative
	gosub HEADER_PAGE
	goto REPORT
 
HEADER_PAGE: !
	pg=pg+1
	if print_details$="N" then temp$(1)="No Details"
	if print_grand_totals$="N" then temp$(3)="No Usage Totals"
L670: form pos 1,c 20,x 5,cc 40,cr 20,skip 1
L680: form pos 1,cc 30,cc 30,cc 30,skip 1
L690: form pos 1,cc 10,x 1,cc 30,x 1,cc 10
	pr #255: ""
	pr #255,using L670: "Company Number [cno]",cnam$,"Page "&str$(pg)
	pr #255,using L680: temp$(1),cap$,temp$(3)
	pr #255,using L740: "Billing Date: "&cnvrt$("pic(zz/zz/zz)",d1)
L740: form pos 30,cc 30,skip 1
	pr #255: ""
	gosub HEADER_COLUMN
return
 
HEADER_COLUMN: !
	if print_details$="N" then goto L830
	pr #255,using L690: "Act.Number","Customer Name","WaterUsage"
	pr #255,using L690: "__________","______________________________","__________"
L830: return
 
PGOF: !
	pr #255: newpage
	if no_more_header=0 then gosub HEADER_PAGE
	no_more_header=0
	continue
 
REPORT: !
	read #7,using L930: r6 eof GRANDTOTAL
L930: form pos 1,pd 3
	read #6,using L470,rec=r6: d3,z$,e2$ eof GRANDTOTAL
	if subtotal_break_check<>d3 and not_first_rec=1 then gosub SUBTOTAL
	subtotal_count+=1
	subtotal_d3+=d3
	if print_details$="Y" then gosub DETAILS
	subtotal_break_check=d3 ! vaL(Z$(1:2))
	not_first_rec=1
	goto REPORT
 
DETAILS: !
	pr #255,using L1050: z$,e2$,d3 pageoflow PGOF
L1050: form pos 1,c 10,x 1,c 30,x 1,n 10
return
 
return
 
SUBTOTAL: !
	if print_details$="N" then pr #255,using L1120: "SubTotals (for usage of "&str$(subtotal_break_check)&")","Customer Count: "&str$(subtotal_count),"Water Usage: "&str$(subtotal_d3) pageoflow PGOF: goto L1180
L1120: form pos 1,c 32,x 3,c 22,x 3,c 26,skip 1
	pr #255,using L690: "__________","______________________________","__________" pageoflow PGOF
	pr #255,using L1150: "SubTotals (for usage of "&str$(subtotal_break_check)&")" pageoflow PGOF
L1150: form pos 1,c 80
	pr #255,using L1150: "Customer Count: "&str$(subtotal_count) pageoflow PGOF
	pr #255,using L1150: "Water Usage: "&str$(subtotal_d3) pageoflow PGOF
L1180: pr #255: "" pageoflow PGOF
	grandtotal_count+=subtotal_count
	grandtotal_d3+=subtotal_d3
	subtotal_count=subtotal_d3=0
	if no_more_header=0 then gosub HEADER_COLUMN
return
 
GRANDTOTAL: !
	no_more_header=1
	gosub SUBTOTAL
	if print_grand_totals$="N" then goto L1340
	pr #255,using L1150: "____________________________________________________" pageoflow PGOF
	pr #255,using L1150: "Grand Totals"
	pr #255,using L1150: "Customer Count: "&str$(grandtotal_count) pageoflow PGOF
	pr #255,using L1150: "Water Usage: "&str$(grandtotal_d3) pageoflow PGOF
	pr #255,using L1150: "____________________________________________________" pageoflow PGOF
L1340: goto DONE
 
