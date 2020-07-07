! Replace S:\acsGL\Reverse
 
	autoLibrary
	fnTop(program$,cap$="Generate Reversing Entries")
	on error goto Ertn
 
	dim cnam$*40,cap$*128,wait_message$*40
	dim t$*12,n(2),l$*12,adr(2),ta(2),p$*30,my_p$*30,test_p$*30,io1$(7)*25
 
	wait_message$="Generating Reversing Entries"
 
	my_p$="Generated Reversing Entry"
 
MENU1: !
	fnTos(sn$="Reverse") : _
	mylen=20: mypos=mylen+3 : right=1
	fnLbl(1,10,"Search For (blank for all)")
	fnLbl(2,1,"Adjustment Date:",mylen,right)
	fnTxt(2,mypos,8,0,right,"1",0,"If you are wanting to reverse some specific adjustments and can identify them by date, use that date.",0 ) : _
	resp$(1)=str$(s_ad)
	fnLbl(3,1,"Reference Number:",mylen,right)
	fnTxt(3,mypos,12,0,right,"",0,"Enter the reference # of the adjustment to reverse a specific adjustment.",0 ) : _
	resp$(2)=s_rn$
	fnLbl(4,1,"Transaction Code:",mylen,right)
	fnTxt(4,mypos,1,0,right,"30",0,"If you wish to reverse all adjustments, you can enter a transaction code of 3.",0 ) : _
	resp$(3)=str$(s_tc)
	fnChk(6,mypos,"Search History Also:",right) : _
	resp$(4)=sh$
	fnLbl(8,5,"Reverse With (blank for no change)")
	fnLbl(9,1,"Adjustment Date:",mylen,right)
	fnTxt(9,mypos,8,0,right,"1",0,"",0 ) : _
	resp$(5)=str$(r_ad)
	fnLbl(10,1,"Reference Number:",mylen,right)
	fnTxt(10,mypos,12,0,right,"",0,"",0 ) : _
	resp$(6)=r_rn$
	fnChk(11,mypos,"Reverse Entry Now:",right) : _
	resp$(7)="True"
	fnCmdKey("&Next",1,1,0,"Proceed with reversing adjustments.")
	fnCmdKey("&Cancel",5,0,1,"Return to menu without reversing.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	s_ad=val(resp$(1))
	s_rn$=resp$(2)
	s_tc=val(resp$(3))
	if resp$(4)="True" then sh$="Y" else sh$="N"
	r_ad=val(resp$(5))
	r_rn$=resp$(6)
	if resp$(7)="True" then re$="Y" else re$="N"
	s_rn$=uprc$(rtrm$(ltrm$(s_rn$)))
 
	glmstr=1 : _
	open #glmstr: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
	gltrans=2 : _
	open #gltrans: "Name=[Q]\GLmstr\GLTrans.h[cno],Shr",internal,outIn,relative
	actrans=3 : _
	open #actrans: "Name=[Q]\GLmstr\AcTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno],Shr",internal,outIn,keyed
	x=lrec(2)
! 
	for j=1 to x
READ_GLTRANS: !
		read #gltrans,using L520,rec=j: t$,s,k,mat n,l$,p$ eof HIST noRec NEXT_GLTRANS
L520: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
		gosub SEARCH_FOR : if s_pass=0 then goto NEXT_GLTRANS
		gosub REVERSE_WITH
		read #glmstr,using L560,key=t$: cb,mat ta nokey NEXT_GLTRANS
L560: form pos 87,pd 6.2,pos 333,2*pd 3
L570: lr2=lrec(2)+1
		write #gltrans,using L520,rec=lr2: t$,s,k,mat n,l$,mp_p$,0 duprec L570
		if ta(1)=0 then ta(1)=lr2
		if ta(2)>0 then : _
			rewrite #gltrans,using L640,rec=ta(2): lr2
		ta(2)=lr2
		cb=cb+k
		rewrite #glmstr,using L560,key=t$: cb,mat ta
L640: form pos 71,pd 3
NEXT_GLTRANS: next j
 
HIST: if sh$="N" then goto DONE
READ_ACTRANS: read #actrans,using L520: t$,s,k,mat n,l$,p$ eof DONE
	gosub SEARCH_FOR : if s_pass=0 then goto NXT_HIST
	gosub REVERSE_WITH
	read #glmstr,using L560,key=t$: cb,mat ta nokey NXT_HIST
L720: lr2=lrec(2)+1
	write #gltrans,using L520,rec=lr2: t$,s,k,mat n,l$,my_p$,0 duprec L720
	if ta(1)=0 then ta(1)=lr2
	if ta(2)>0 then rewrite #gltrans,using L640,rec=ta(2): lr2
	ta(2)=lr2
	cb=cb+k
	rewrite #glmstr,using L560,key=t$: cb,mat ta
NXT_HIST: goto READ_ACTRANS
 
DONE: !
	close #win: ioerr L830
L830: goto Xit
 
Xit: fnXit
 
include: Ertn
 
SEARCH_FOR: !
 
	test_p$=uprc$(rtrm$(ltrm$(p$)))
	test_l$=uprc$(rtrm$(ltrm$(l$)))
 
	if test_p$<>uprc$(rtrm$(ltrm$(my_p$))) then s_test_p=1
	if test_p$=uprc$(rtrm$(ltrm$(my_p$))) then s_test_p=0
	if s_ad=0 then s_test_ad=1
	if s_ad<>0 and s_ad=s then s_test_ad=1
	if s_ad<>0 and s_ad<>s then s_test_ad=0
	if s_tc=0 then s_test_tc=1
	if s_tc<>0 and s_tc=n(1) then s_test_tc=1
	if s_tc<>0 and s_tc<>n(1) then s_test_tc=0
	if s_rn$="" then s_test_rn=1
	if s_rn$<>"" and s_rn$=test_l$ then s_test_rn=1
	if s_rn$<>"" and s_rn$<>test_l$ then s_test_rn=0
	if s_test_ad=1 and s_test_tc=1 and s_test_rn=1 and s_test_p=1 then : _
		s_pass=1 !      Pass                            : _
	else s_pass=0 ! Fail
return
 
REVERSE_WITH: !
	if re$="Y" then k=-k
	if r_ad<>0 then s=r_ad
	if r_rn$<>"" then l$=r_rn$
	if t$(3:3)=" " then t$(3:3)="0"
	if t$(12:12)=" " then t$(12:12)="0"
return
 
