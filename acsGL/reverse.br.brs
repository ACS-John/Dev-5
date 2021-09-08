! Replace S:\acsGL\Reverse

autoLibrary
fnTop(program$,"Generate Reversing Entries")
on error goto Ertn

dim my_p$*30
my_p$="Generated Reversing Entry"

MENU1: !
	fnTos
	mylen=20: mypos=mylen+3
	fnLbl(1,10,"Search For (blank for all)")
	fnLbl(2,1,"Adjustment Date:",mylen,1)
	fnTxt(2,mypos,8,0,1,"1",0,"If you are wanting to reverse some specific adjustments and can identify them by date, use that date.",0 )
	resp$(1)=str$(s_ad)
	fnLbl(3,1,"Reference Number:",mylen,1)
	fnTxt(3,mypos,12,0,1,"",0,"Enter the reference # of the adjustment to reverse a specific adjustment.",0 )
	resp$(2)=s_rn$
	fnLbl(4,1,"Transaction Type:",mylen,1)
	fnTxt(4,mypos,1,0,1,"30",0,"If you wish to reverse all adjustments, you can enter a transaction code of 3.",0 )
	resp$(3)=str$(s_tc)
	fnChk(6,mypos,"Search History Also:",1)
	resp$(4)=sh$
	fnLbl(8,5,"Reverse With (blank for no change)")
	fnLbl(9,1,"Adjustment Date:",mylen,1)
	fnTxt(9,mypos,8,0,1,"1",0,"",0 )
	resp$(5)=str$(r_ad)
	fnLbl(10,1,"Reference Number:",mylen,1)
	fnTxt(10,mypos,12,0,1,"",0,"",0 )
	resp$(6)=r_rn$
	fnChk(11,mypos,"Reverse Entry Now:",1)
	resp$(7)="True"
	fnCmdKey("&Next",1,1,0,"Proceed with reversing adjustments.")
	fnCmdKey("&Cancel",5,0,1,"Return to menu without reversing.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	s_ad=val(resp$(1))
	s_rn$=resp$(2)
	s_tc=val(resp$(3))
	if resp$(4)="True" then sh$="Y" else sh$="N"
	r_ad=val(resp$(5))
	r_rn$=resp$(6)
	if resp$(7)="True" then re$="Y" else re$="N"
	s_rn$=uprc$(rtrm$(ltrm$(s_rn$)))

	open #glmstr=1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
	open #gltrans=fnH: 'Name=[Q]\GLmstr\GLTrans.h[cno],kfname=[Q]\GLmstr\glTrans-IdxAcct.h[cno],Shr',internal,outIn,keyed
	open #actrans=3: "Name=[Q]\GLmstr\AcTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno],Shr",internal,outIn,keyed
	x=lrec(gltrans)

	for j=1 to x
		READ_GLTRANS: !
		dim t$*12
		dim nx(2)
		dim l$*12
		dim p$*30
		read #gltrans,using L520,rec=j: t$,s,k,mat nx,l$,p$ eof HIST noRec NEXT_GLTRANS
		L520: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
		gosub SEARCH_FOR : if s_pass=0 then goto NEXT_GLTRANS
		gosub REVERSE_WITH
		dim ta(2)
		read #glmstr,using L560,key=t$: cb,mat ta nokey NEXT_GLTRANS
		L560: form pos 87,pd 6.2,pos 333,2*pd 3
		L570: !
		lr2=lrec(gltrans)+1
		write #gltrans,using L520,rec=lr2: t$,s,k,mat nx,l$,mp_p$,0 duprec L570
		if ta(1)=0 then ta(1)=lr2
		if ta(2)>0 then
			rewrite #gltrans,using L640,rec=ta(2): lr2
			L640: form pos 71,pd 3
		end if
		ta(2)=lr2
		cb=cb+k
		rewrite #glmstr,using L560,key=t$: cb,mat ta
		NEXT_GLTRANS: !
	next j

HIST: !
if sh$="N" then goto DONE
do
	read #actrans,using L520: t$,s,k,mat nx,l$,p$ eof DONE
	gosub SEARCH_FOR : if s_pass=0 then goto NXT_HIST
	gosub REVERSE_WITH
	read #glmstr,using L560,key=t$: cb,mat ta nokey NXT_HIST
	L720: !
	lr2=lrec(gltrans)+1
	write #gltrans,using L520,rec=lr2: t$,s,k,mat nx,l$,my_p$,0 duprec L720
	if ta(1)=0 then ta(1)=lr2
	if ta(2)>0 then rewrite #gltrans,using L640,rec=ta(2): lr2
	ta(2)=lr2
	cb=cb+k
	rewrite #glmstr,using L560,key=t$: cb,mat ta
	NXT_HIST: !
loop

DONE: !
	close #win: ioerr ignore
goto Xit

Xit: fnXit

SEARCH_FOR: !

	dim test_p$*30
	test_p$=uprc$(rtrm$(ltrm$(p$)))
	test_l$=uprc$(rtrm$(ltrm$(l$)))

	if test_p$<>uprc$(rtrm$(ltrm$(my_p$))) then s_test_p=1
	if test_p$=uprc$(rtrm$(ltrm$(my_p$))) then s_test_p=0
	if s_ad=0 then s_test_ad=1
	if s_ad<>0 and s_ad=s then s_test_ad=1
	if s_ad<>0 and s_ad<>s then s_test_ad=0
	if s_tc=0 then s_test_tc=1
	if s_tc<>0 and s_tc=nx(1) then s_test_tc=1
	if s_tc<>0 and s_tc<>nx(1) then s_test_tc=0
	if s_rn$="" then s_test_rn=1
	if s_rn$<>"" and s_rn$=test_l$ then s_test_rn=1
	if s_rn$<>"" and s_rn$<>test_l$ then s_test_rn=0
	if s_test_ad=1 and s_test_tc=1 and s_test_rn=1 and s_test_p=1 then
		s_pass=1 ! Pass
	else
		s_pass=0 ! Fail
	end if
return

REVERSE_WITH: ! r:
	if re$="Y" then k=-k
	if r_ad<>0 then s=r_ad
	if r_rn$<>"" then l$=r_rn$
	if t$(3:3)=" " then t$(3:3)="0"
	if t$(12:12)=" " then t$(12:12)="0"
return ! /r

include: ertn