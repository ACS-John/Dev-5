Execute "config gui off"
autoLibrary
on error goto Ertn
a=1
do
	Open #20: "name=pdf:/,recl=512,replace",display,output
	pr #20: STR$(A)
	Close #20: ioerr ignore
	pr STR$(A)
	A+=1
loop while A<500
Xit: end
! /r
include: ertn