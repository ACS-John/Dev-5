! Replace S:\acsGL\PRClose
! GENERAL LEDGER Payroll Only Month End Closing

autoLibrary
on error goto Ertn

fnTop(program$,"Payroll Only Month End Closing")
! fnwait - "GENERAL LEDGER Payroll Only Month End Closing IN PROCESS"
! empty the General Ledger Payroll Checks File
open #20: "Name=[Q]\GLmstr\ACPRCKS.h[cno],Size=0,RecL=110,Replace",internal,output: close #20:
open #prmstr=1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],NoShr",internal,outIn,keyed
READ_PRMSTR: !
	read #prmstr,using 'Form POS 271,2*N 5': n1,n2 eof DONE
	rewrite #prmstr,using 'Form POS 271,2*N 5': 0,0
goto READ_PRMSTR

DONE: !
	close #prmstr:
goto Xit

Xit: fnXit
include: ertn No

