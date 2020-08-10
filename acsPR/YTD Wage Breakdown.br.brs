! formerly S:\acsPR\prYTDPay
! PR Year To Date Pay Report
 
autoLibrary
fnTop(program$)
on error goto Ertn

dim dat$*20
dim em1$*30
dim tdet(17),tdy(6),tdc(6),ty(21)

fndat(dat$)
open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed
open #2: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",internal,input,relative
fnopenprn
gosub HDR
goto LYNES
 
NPG: ! r:
	pr #255: newpage
	gosub HDR
continue ! /r
HDR: ! r:
	pr #255,using L240: "PR Year To Date Pay Report","Page",pagenum+=1,env$('cnam'),dat$
	L240: form pos 53,c 40,pos 122,c 6,n 3,skip 1,cc 132,skip 1,cc 132,skip 2
	pr #255,using L250: "Emp-Numb","Name                          ","     Reg Wages","   Overtime","  Other Cmp","     Vacation","         Sick","      Holiday","      Total"
	L250: form pos 1,c 8,x 1,c 30,x 1,c 14,x 1,c 11,x 1,c 11,x 1,c 13,x 1,c 13,x 1,c 13,x 1,c 11,skip 1
	pr #255,using L250: "________","______________________________","______________","___________","___________","_____________","_____________","_____________","___________"
return ! /r
LYNES: ! r:
	em1$=""
	c3=0
	c4=0
	c5=0
	c6=0
	c7=0
	c8=0
	c9=0
	do
		read #2,using L380: teno,tli,mat tdet,mat tdy,mat tdc,mat ty,nta eof SUMMARY
		L380: form pos 1,n 8,pos 54,24*pd 4.2,6*pd 3.2,21*pd 5.2,pos 468,pd 3
		L390: form pos 9,c 30
		! r: CALC
		c4+=ty(17)
		c5+=ty(18)
		c6+=tdet(2)*tdy(4)
		c7+=tdy(3)*tdet(2)
		c8+=tdy(5)*tdet(2)
		c9+=ty(21)
		c3+=ty(21)-c6-c7-c8
		! /r
	loop while nta
	
	read #1,using L390,key=lpad$(str$(teno),8): em1$ nokey LYNES
	! r: calc total(s)
	tc3+=c3
	tc4+=c4
	tc5+=c5
	tc6+=c6
	tc7+=c7
	tc8+=c8
	tc9+=c9
	! /r
	pr #255,using L260: teno,em1$,round(c3,2),round(c4,2),round(c5,2),round(c6,2),round(c7,2),round(c8,2),round(c9,2) pageoflow NPG
	L260: form pos 1,n 8,x 1,c 30,x 1,n 14.2,x 1,n 11.2,x 1,n 11.2,x 1,n 13.2,x 1,n 13.2,x 1,n 13.2,x 1,n 11.2,skip 1
goto LYNES ! /r
 
SUMMARY: ! r:
	pr #255: ""
	pr #255,using L710: "Total Regular Wages:",tc3
	pr #255,using L710: "      Overtime Wages:",tc4
	pr #255,using L710: "      Other Compensation:",tc5
	pr #255,using L710: "      Vacation Wages:",tc6
	pr #255,using L710: "      Sick Wages:",tc7
	pr #255,using L710: "      Holiday Wages:",tc8
	pr #255: ""
	pr #255,using L710: "      Total Wages:",tc9
	L710: form pos 10,c 25,pos 37,n 19.2
! /r
	fncloseprn
Xit: fnXit
include: Ertn
