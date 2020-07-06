! Replace S:\acsGL\BldRange
! Build Range of Accounts
 
	autoLibrary
	on error goto Ertn
 
	dim io1$(12),gln(3,3),fin(3),ta(2),ac(18),te$*1
	dim d$*50,bc(13),bp(13),bm(13),rf(6),glk$*12,fsk$*5,resp$(20)*50
 
	! fnconsole(off=0)
	fnTop(program$,"Duplicate Range of Accounts")
	open #company=1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
	read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read fund and sub codes from general
	close #company:
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
	fil$(1)="ACGLFNSB" : idx$(1)="agfsidx4"
	fil$(2)="ACGLFNSI" : idx$(2)="agfsidx3"
 
MAIN: !
	fnTos(sn$="Bldrange")
	mylen=40: mypos=mylen+3 : right=1: rc=0
	fnFra(1,1,6,90,"Duplicate Range of Accounts","This option will allow you to quickly duplicate a range of general ledger numbers when setting up multiple departments with similar accounts. ",0)
	fnLbl(1,50,"Source",0,0,0,1)
	fnLbl(2,1,"1st G/L Number to Duplicate:",mylen,right,0,1)
	fnqgl(2,mypos,1,0)
	resp$(rc+=1)=fnrgl$(gl1$)
	fnLbl(3,1,"Last G/L Number to Duplicate:",mylen,right,0,1)
	fnqgl(3,mypos,1,0)
	resp$(rc+=1)=fnrgl$(gl2$)
	fnLbl(6,1,"First new general ledger # to be used:",mylen,right,0,1)
	fnLbl(5,44,"Fund #",6,1,0,1)
	fnLbl(5,58,"Sub #",6,2,0,1)
	if use_dept=1 then let fnTxt(6,46,3,0,right,"30",0,"Enter the fund portion of the general ledger number.",1 ) else let fnTxt(6,46,3,0,right,"30",1,"Enter the fund portion of the general ledger number.",1 ) ! : rESP$(RC+=1)=STR$(DNO)
	fnTxt(6,51,6,0,right,"30",0,"Enter the main part of the general ledger number.",1 )
	resp$(rc+=1)=""
	if use_sub=1 then let fnTxt(6,60,3,0,right,"30",0,"Enter the sub portion of the general ledger number.",1 ) else let fnTxt(6,60,3,0,right,"30",1,"Enter the sub portion of the general ledger number.",1 )
		resp$(rc+=1)=""
	fnFra(9,1,8,125,"Duplicating Matching Range of Financial Statement Formats"," ",0)
 
	fnLbl(1,1,"Beginning                         Ending",90,right,0,2)
	fnLbl(2,1,"Balance Sheet Refernece Number:",mylen,right,0,2)
	fncombof("fs-bal",2,43,25,"[Q]\GLmstr\acglfnsb.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx4.h[cno]",0,pas, "If the accounts you are duplicating are balance sheet accounts, select the beginning balance sheet reference number to match the first new balance sheet account.",2)
	resp$(rc+=1)="" ! first balance sheet ref # to be duplicated
	fncombof("fs-bal2",2,85,25,"[Q]\GLmstr\acglfnsb.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx4.h[cno]",0,pas, "Select the last balance sheet reference number to be duplicated.",2)
	resp$(5)="" ! ending balance sheet ref # to be duplicated
	fnLbl(4,1,"Beginning                         Ending",90,right,0,2)
	fnLbl(5,1,"Income Statement Refernece Number:",mylen,right,0,2)
	fncombof("fs-inc",5,43,25,"[Q]\GLmstr\acglfnsi.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx3.h[cno]",0,pas, "If you are duplicating income statement accounts, enter the first income statement reference to be duplicated.",2)
	resp$(rc+=1)="" ! 1st income statement ref # to be duplicated
	fncombof("fs-inc-2",5,85,25,"[Q]\GLmstr\acglfnsi.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx3.h[cno]",0,pas, "If you are duplicating income statement accounts, enter the last income statement reference to be duplicated.",2)
	resp$(rc+=1)="" ! last income statement ref # to be duplicated
	fnLbl(7,1,"First new reference # to be used:",mylen,right,0,2)
	fnTxt(7,mylen+3,5,0,right,"30",0,"Enter the first new financial statement reference number to be matched with the new general ledger numbers.",2 )
	resp$(rc+=1)=""
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	pas=0 ! rebuild each time
	if ckey=5 then goto Xit
	gl$=fnagl$(resp$(1))
	gln(1,1)=val(gl$(1:3))
	gln(1,2)=val(gl$(4:9))
	gln(1,3)=val(gl$(10:12))
	gl$=fnagl$(resp$(2))
	gln(2,1)=val(gl$(1:3))
	gln(2,2)=val(gl$(4:9))
	gln(2,3)=val(gl$(10:12))
	gl$=cnvrt$("pic(Zz#)",val(resp$(3)))&cnvrt$("pic(zzzZz#)",val(resp$(4)))&cnvrt$("PIC(ZZ#)",val(resp$(5)))
	gln(3,1)=val(gl$(1:3))
	gln(3,2)=val(gl$(4:9))
	gln(3,3)=val(gl$(10:12))
	if val(resp$(6)(1:6))> 0 then fin(1)=val(resp$(6)(1:6))
	if val(resp$(7)(1:6))> 0 then fin(2)=val(resp$(7)(1:6))
	if val(resp$(8)(1:6))> 0 then fin(1)=val(resp$(8)(1:6))
	if val(resp$(9)(1:6))> 0 then fin(2)=val(resp$(9)(1:6))
	fin(3)=val(resp$(10))
	gl1=val(cnvrt$("PIC(###)",gln(1,1))&cnvrt$("PIC(######)",gln(1,2))&cnvrt$("PIC(###)",gln(1,3)))
	gl2=val(cnvrt$("PIC(###)",gln(2,1))&cnvrt$("PIC(######)",gln(2,2))&cnvrt$("PIC(###)",gln(2,3)))
	gl3=val(cnvrt$("PIC(###)",gln(3,1))&cnvrt$("PIC(######)",gln(3,2))&cnvrt$("PIC(###)",gln(3,3)))
	if gl1=0 then goto MAIN
	if gl2=0 then goto MAIN
	if gl3=0 then goto MAIN
	gf=gl3-gl1
	glk$=lpad$(str$(gln(1,1)),3)&lpad$(str$(gln(1,2)),6)&lpad$(str$(gln(1,3)),3)
	if fin(1)=0 then goto L990
	if fin(2)=0 then goto MAIN
	if fin(3)=0 then goto MAIN
 
	ff=fin(3)-fin(1)
	restore #1,key=glk$: nokey MAIN
L830: read #1,using L1030: dno,ano,sno,d$,mat rf eof MAIN
	gl=val(cnvrt$("PIC(###)",dno)&cnvrt$("PIC(######)",ano)&cnvrt$("PIC(###)",sno))
	if gl>gl2 then goto MAIN
	if rf(1)>0 then fln=1: goto L890
	if rf(3)>0 then fln=2: goto L890
	goto L830
L890: open #2: "Name=[Q]\GLmstr\"&fil$(fln)&".h[cno],KFName=[Q]\GLmstr\"&idx$(fln)&".h[cno],Shr",internal,outIn,keyed
	restore #2,key=lpad$(str$(fin(1)),5): nokey L970
L910: read #2,using L920: rno,d$,te$,mat ac eof L980
L920: form pos 1,n 5,c 50,c 1,2*n 2,15*n 1,n 3
	if rno>fin(2) then goto L980
	rno=rno+ff
	write #2,using L920: rno,d$,te$,mat ac
	goto L910
L970: close #2: : goto MAIN
L980: close #2:
L990: restore #1,key=glk$: nokey L1010
	goto L1020
L1010: goto MAIN
L1020: read #1,using L1030: dno,ano,sno,d$,mat rf eof END1
L1030: form pos 1,n 3,n 6,n 3,c 50,6*pd 3,42*pd 6.2,2*pd 3
	gl=val(cnvrt$("PIC(###)",dno)&cnvrt$("PIC(######)",ano)&cnvrt$("PIC(###)",sno))
	if gl>gl2 then goto END1
	gl=gl+gf
	gl$=lpad$(str$(gl),12)
	dno=val(gl$(1:3))
	ano=val(gl$(4:9))
	sno=val(gl$(10:12))
	if fln=1 then rf(1)=rf(1)+ff
	if fln=2 then rf(3)=rf(3)+ff
	write #1,using L1030: dno,ano,sno,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta
	goto L1020
 
END1: !
	close #1:
	close #2: ioerr ignore
	! Execute "Index [Q]\GLmstr\GLmstr.h[cno],[Q]\GLmstr\GLIndex.h[cno],1,12,Replace,DupKeys -n"
	if fln>0 then execute "Index [Q]\GLmstr\"&fil$(fln)&".h[cno],[Q]\GLmstr\"&idx$(fln)&".h[cno],1,5,Replace,DupKeys -n"
	goto MAIN
 
Xit: fnXit
 
include: Ertn
