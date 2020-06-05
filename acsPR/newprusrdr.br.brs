! Replace S:\acsPR\newPrUsrDR
! pr User-Designed Reports
 
	autoLibrary
	on error goto Ertn
 
	dim jcs$(40),cap$*128,rn(20),rn$(20)*74,resp$(5)*90,df$*256,if$*256
 
	fnTop(program$,cap$="User Designed Report")
MAIN_SCREEN: !
	fnTos(sn$="user1") : _
	mylen=25 : mypos=mylen+2: resp=0: left=1
	df$="S:\acsPR\Jcreport.mst" : if$="S:\acsPR\jcreport.idx" : _
	fncombof("CRjcreport",1,1,80,df$,1,2,3,74,if$,1) : _
	fncombof("CRjcreportALL",1,1,80,df$,1,2,3,74,if$,2)
	resp$(1)=""
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	rno=val(resp$(1)(1:2))
 
JCPRNT: chain "S:\acsPR\jcPrnt"&str$(rno)
 
Xit: fnXit
 
include: Ertn
 
