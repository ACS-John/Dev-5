! Replace S:\acsPR\newPrUsrDR
! pr User-Designed Reports

autoLibrary
on error goto Ertn

fnTop(program$,"User Designed Report")
MAIN_SCREEN: !
	dim resp$(5)*90
	fnTos
	mylen=25 : mypos=mylen+2: resp=0: left=1
	fncombof("CRjcreport"   ,1,1,80,"S:\acsPR\Jcreport.mst",1,2,3,74,"S:\acsPR\jcreport.idx" ,1) 
	fncombof("CRjcreportALL",1,1,80,"S:\acsPR\Jcreport.mst",1,2,3,74,"S:\acsPR\jcreport.idx" ,2)
	resp$(1)=""
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	rno=val(resp$(1)(1:2))
 
chain "S:\acsPR\jcPrnt"&str$(rno)
 
Xit: fnXit
 
include: ertn
 
