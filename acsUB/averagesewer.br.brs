! average sewer usage program
	autoLibrary
	dim cap$*128,sendto$*80,z$*10,e$(4)*30,temp$(3)*26
	fnTop("S:\Utility Billing\Billing Journal",cap$="Average Sewer Usage Report")
	fncno(cno,cnam$)
 
	on fkey 5 goto DONE
	fnTos(sn$="sewerusage")
	fnLbl(1,5,"Show Customers who have sewer usage")
	fnLbl(2,1,"Greater than:",20,1)
	fnTxt(2,23,9,0,right,"30",0,"Enter the minimum usage you want cosidered in your report. (Blank for all)",0 ) : _
	resp$(2)=""
	fnLbl(3,1,"Less than:",20,1)
	fnTxt(3,23,9,0,right,"30",0,"Enter the maximum usage you want cosidered in your report (Blank for all).",0 ) : _
	resp$(3)=""
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	minu=val(resp$(1))
	maxu=val(resp$(2))
	goto STARTREPORT
 
DONE: !
	fncloseprn
Xit: fnXit
 
STARTREPORT: !
! maybe a printing please wait screen here would be nice.
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
	fnopenprn
	gosub HEADER
	goto REPORT
 
HEADER: !
	pg=pg+1
L360: form pos 1,c 20,cc 40,cr 20,skip 1
	form pos 1,cc 26,cc 26,cc 26,skip 1
L380: form pos 1,cc 10,x 1,cc 30,x 1,cc 10,x 1,cc 17,x 1,cc 10,skip 1
	pr #255,using L360: "",cap$,""
	pr #255,using L360: "Company Number [cno]",cnam$,"Page "&str$(pg)
	pr #255: ""
	pr #255: ""
	pr #255,using L380: "Act.Number","Customer Name","Balance","Last Billing Date","Average"
	pr #255,using L380: "__________","______________________________","__________","_________________","__________"
return
 
PGOF: !
	pr #255: newpage
	gosub HEADER
	continue
 
REPORT: !
L530: read #1,using L540: z$,mat e$,final,average,bal,lastbilldate eof DONE
L540: form pos 1,c 10,4*c 30,pos 1821,n 1,pos 1822,n 9,pos 292,pd 4.2,pd 4
	if minu=0 and maxu=0 then goto L570
	if (minu>0 and average<minu) or (maxu>0 and average>maxu) then goto L530 else goto L570
L570: pr #255,using L580: z$,e$(2),bal,lastbilldate,average pageoflow PGOF
L580: form pos 1,c 10,x 1,c 30,x 1,n 10.2,x 1,pic(zz/zz/zz),x 8,n 10
	goto L530
 
