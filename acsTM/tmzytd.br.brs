
	on error goto Ertn
	library 'S:\Core\Library': fntop
	library 'S:\Core\Library': fnxit
	library 'S:\Core\Library': fnchain
	fntop(program$,"Zero Year to Date")

	dim eno$*9,cytdhrs(10),ncytdhrs(20),cytdamt(10),ncytdamt(20),prg$*20
Scr1: ! r:
	pr newpage
	pr f "8,25,c 30,r,n": "********  WARNING  ********"
	pr f "11,5,C 75": "THIS PROGRAM ZEROS ALL YEAR-TO-DATE FIELDS IN THE EMPLOYEE MASTER FILE"
	pr f "12,5,c 75": "AND THE SERVICE CODE FILE.  IT ALSO CLEARS THE YTD BILLING SUMMARY FILE."
	pr f "14,8,c 70": "BE SURE YOU HAVE RUN ALL OF THE YEAR-END REPORTS BEFORE CONTINUING!"
	pr f "16,8,c 62": "ENTER 1 TO CONTINUE; ELSE ENTER 2 TO RETURN TO THE SYSTEM MENU"
	L180: !
	input fields "16,75,n 1,uE,n": a conv L180
on a goto L200,END1 none Scr1
! /r
L200: !
pr newpage
	pr f "10,20,C 60,H,N": "ZERO YEAR TO DATE INFORMATION IN PROCESS"
	open #1: "Name=S:\Core\Data\acsllc\EMmstr.h[cno],KFName=S:\Core\Data\acsllc\EMINDEX.h[cno],Shr",internal,outIn,keyed
	do
		read #1,using 'form pos 1,c 9': eno$ eof Eo1
		rewrite #1,using 'form pos 158,30*pd 4.2,pos 428,30*pd 5.2': mat cytdhrs,mat ncytdhrs,mat cytdamt,mat ncytdamt
	loop
	Eo1: !
	close #1: 
	open #1: "Name=S:\Core\Data\acsllc\SCMSTR.H[cno],KFName=S:\Core\Data\acsllc\SCIndex.H[cno],Shr",internal,outIn,keyed
	F_Scmstr: form pos 35,pd 4.2,pd 5.2
	do
		read #1,using F_Scmstr: a1 eof EoScmstr
		rewrite #1,using F_Scmstr: 0,0
	loop
	EoScmstr: !
	close #1: 
END1: !
if uprc$(rtrm$(prg$))="S:\acsTM\EMAINT" then fnchain("S:\acsTM\EMAINT")
XIT: fnxit
include: Ertn
