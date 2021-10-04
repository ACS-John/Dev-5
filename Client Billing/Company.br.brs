autoLibrary
fnTop(program$)
on error goto Ertn

open #hCompany=fnH: "Name=S:\Core\Data\acsllc\Company.h[cno],RecL=245",i,outi,r
Fcompany: form pos 1,4*c 40,4*n 1,pd 3.3,5*pd 2,n 3,n 6,n 3,n 3,n 6,n 3,n 3,n 6,n 3,n 3,n 6,n 3,c 20
dim co$(4)*40
dim cox(6)
dim ag(4)
dim gln(4,3)
dim dat$*20

if lrec(hCompany)=0 then
	write #hCompany,using Fcompany,rec=1: mat co$,mat cox,mat ag,mat gln,dat$
end if

read #hCompany,using Fcompany,rec=1: mat co$,mat cox,mat ag,mat gln,dat$
pr newpage

pr f '2 ,2,C 66': 'Company Name'
pr f '3 ,2,C 66': 'Co Address 1'
pr f '4 ,2,C 66': 'Co Address 2'
pr f '5 ,2,C 66': 'City State Zip'
pr f '6 ,2,C 66': 'GL Installed                                             (1=Yes)'
pr f '7 ,2,C 66': 'Fund Number Used                                         (1=Yes)'
pr f '8 ,2,C 66': 'Sub Account Used                                         (1=Yes)'
pr f '9 ,2,C 66': 'Print Details on Statements                              (1=Yes)'
pr f '10,2,C 66': 'Finance Charge Rate                        (Enter 10% as 10.000)'
pr f '11,2,C 66': '# of Days Before Fin Chg Applicable'
pr f '12,2,C 66': 'Aging Period 1'
pr f '13,2,C 66': 'Aging Period 2'
pr f '14,2,C 66': 'Aging Period 3'
pr f '15,2,C 66': 'Aging Period 4'
pr f '16,2,C 66': 'GL # Cash'
pr f '17,2,C 66': 'GL # AR'
pr f '18,2,C 66': 'GL # Finance Charge'
pr f '19,2,C 66': 'GL # Standard Charge'
pr f '20,2,C 66': 'Report Heading Date'
! r: build mat io1$
	dim io1$(27)
	io1$( 1)=' 2,20,C 40  ,U,N'
	io1$( 2)=' 3,20,C 40  ,U,N'
	io1$( 3)=' 4,20,C 40  ,U,N'
	io1$( 4)=' 5,20,C 40  ,U,N'
	io1$( 5)=' 6,42,N  1  ,U,N'
	io1$( 6)=' 7,42,N  1  ,U,N'
	io1$( 7)=' 8,42,N  1  ,U,N'
	io1$( 8)=' 9,42,N  1  ,U,N'
	io1$( 9)='10,37,N  6.3,U,N'
	io1$(10)='11,40,N  3  ,U,N'
	io1$(11)='12,40,N  3  ,U,N'
	io1$(12)='13,40,N  3  ,U,N'
	io1$(13)='14,40,N  3  ,U,N'
	io1$(14)='15,40,N  3  ,U,N'
	io1$(15)='16,40,N  3  ,U,N'
	io1$(15)='16,30,N  3  ,U,N'
	io1$(16)='16,37,N  6  ,U,N'
	io1$(17)='16,47,N  3  ,U,N'
	io1$(18)='17,30,N  3  ,U,N'
	io1$(19)='17,37,N  6  ,U,N'
	io1$(20)='17,47,N  3  ,U,N'
	io1$(21)='18,30,N  3  ,U,N'
	io1$(22)='18,37,N  6  ,U,N'
	io1$(23)='18,47,N  3  ,U,N'
	io1$(24)='19,30,N  3  ,U,N'
	io1$(25)='19,37,N  6  ,U,N'
	io1$(26)='19,47,N  3  ,U,N'
	io1$(27)='20,30,C 20  ,U,N'
! /r
pr f mat io1$: mat co$,mat cox,mat ag,mat gln,dat$
pr f "23,30,c 40": "Enter to Save   F5 to cancel"
L550: !
input fields mat io1$: mat co$,mat cox,mat ag,mat gln,dat$ conv L670

if cmdkey=5 then goto Xit
if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
if rtrm$(co$(1))="" then ce=1: goto L690
for j=1 to 4
	if cox(j)<0 or cox(j)>1 then ce=j+4: goto L690
	if j=1 then goto L620
	if ag(j-1)>ag(j) then ce=j+10: goto L690
L620: !
next j
if cox(5)<0 or cox(5)>99 then ce=9: goto L690
rewrite #hCompany,using Fcompany,rec=1: mat co$,mat cox,mat ag,mat gln,dat$
close #hCompany:
goto Xit
L670: !
	if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
L690: !
	pr f "24,78,C 1": bell
	io1$(ce)=rtrm$(io1$(ce))
	ce1=pos(uprc$(io1$(ce)),"U",1)
	ce2=ce1+1
	io1$(ce)(ce1:ce1)="CR"
goto L550

Xit: fnXit
include: ertn
