! S:\acsGL\adjustaccounts
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,resp$(3)*255,ymbal(13),priorym(13)
	fnTop(program$,cap$="Adjust Account Balances")
	fn_adjustaccounts
def fn_adjustaccounts
		fncno(cno)
		open #(h_glmstr:=fngethandle): "Name=[Q]\GLmstr\glmstr.h[cno],KFName=[Q]\GLmstr\GLindex.h[cno],Shr",internal,outIn,keyed
GLMSTR: form n 3,n 6,n 3,pos 81,28*pd 6.2
		open #(h_actrans:=fngethandle): "Name=[Q]\GLmstr\actrans.h[cno],KFName=[Q]\GLmstr\ACTRIDX.h[cno],Shr",internal,input,keyed
ACTRANS: form n 3,n 6,n 3,n 6,pd 6.2,pos 71,n 2
		open #(h_gltrans:=fngethandle): "Name=[Q]\GLmstr\gltrans.h[cno],Shr",internal,input
GLTRANS: form n 3,n 6,n 3,n 6,pd 6.2
		do
			fn_getadjustment
			if gl$<>'' then
				read #h_glmstr,using GLMSTR,key=gl$: dept,major,sub,startbal,balance,mat ymbal,mat priorym
				balance=adjustamt
				read #h_actrans,using ACTRANS,key>=rpad$(gl$,20): trdept,trmajor,trsub,trdate,tramt,pcode eof ACTRANSDONE
				do
					if trdept<>dept or trmajor<>major or trsub<>sub then goto ACTRANSDONE
					if date(days(trdate,'mmddyy'),'ccyymmdd')>date(days(adjustdate,'mmddyy'),'ccyymmdd') then
					  if oldpcode>pcode then ! this means we're going back to the first period, so move our period values back into the previous yrmo and clear out current
					    for period=1 to 13
					      priorym(period)=ymbal(period) : ymbal(period)=0
					    next period
					    startbal=priorym(oldpcode)
					  end if
					  oldpcode=pcode
					  balance+=tramt
					  ymbal(pcode)=balance
					end if
					read #h_actrans,using ACTRANS,next: trdept,trmajor,trsub,trdate,tramt,pcode eof ACTRANSDONE
				loop
ACTRANSDONE: restore #h_gltrans:
				do
					read #h_gltrans,using GLTRANS: trdept,trmajor,trsub,trdate,tramt eof GLTRANSDONE
					if date(days(trdate,'mmddyy'),'ccyymmdd')>date(days(adjustdate,'mmddyy'),'ccyymmdd') and trdept=dept and trmajor=major and trsub=sub then
					  balance+=tramt
					end if
				loop
GLTRANSDONE: if fn_confirmadjustment then rewrite #h_glmstr,using GLMSTR: dept,major,sub,startbal,balance,mat ymbal,mat priorym
			end if
		loop while gl$<>''
		close #h_glmstr:
		close #h_actrans:
		close #h_gltrans:
		fnXit
fnend
def fn_getadjustment
		mat resp$(3)
		fnTos("accountadjust")
		mylen=23: mypos=mylen+3 : right=1
		fnLbl(1,1,"General Ledger Number:",mylen,right)
		fnqglbig(1,mypos,0,2) : resp$(1)=fnrglbig$(gl$)
		fnLbl(2,1,"Date of Balance:",mylen,right)
		fnTxt(2,mypos,8,8,1,"1",0) : resp$(2)=cnvrt$("pic(zzzzzz)",adjustdate)
		fnLbl(3,1,"Balance on Date:",mylen,right)
		fnTxt(3,mypos,14,0,right,"10",0) : resp$(3)=''
		fnCmdSet(11)
		fnAcs2(mat resp$,ckey)
		if ckey=1 then
			gl$=fnagl$(resp$(1)) : adjustdate=val(resp$(2)) : adjustamt=val(resp$(3))
		else
			gl$=''
		end if
fnend
def fn_confirmadjustment
		mat resp$(27)
		fnTos("adjustconfirm")
		mylen=23 : mypos=mylen+3 : right=1
		fnLbl(1,1,"Account "&gl$,mylen)
		fnLbl(2,1,"Current Balance:",mylen,right)
		fnTxt(2,mypos,14,0,right,"10",0) : resp$(1)=str$(balance)
		for period=1 to 13
			fnLbl(3+period,1,"Period "&str$(period)&":",mylen,right)
			fnTxt(3+period,mypos,14,0,right,"10",0) : resp$(period+1)=str$(ymbal(period))
		next period
		for period=1 to 13
			fnLbl(3+period,mypos+20,"Prior:",mylen,right)
			fnTxt(3+period,mypos*2+20,14,0,right,"10",0) : resp$(period+14)=str$(priorym(period))
		next period
		fnCmdKey("&Save",1,1)
		fnCmdKey("Clear &Periods",2,0)
		fnCmdKey("&Cancel",5,0,1)
		fnAcs("adjustconfirm",0,mat resp$,ckey)
		if ckey=1 then
			balance=val(resp$(1))
			for period=1 to 13 : ymbal(period)=val(resp$(period+1)) : priorym(period)=val(resp$(period+14)) : next period
			fn_confirmadjustment=1
		else if ckey=2 then
			for period=1 to 13
				priorym(period)=ymbal(period) : ymbal(period)=0
			next period
			startbal=priorym(oldpcode)
			fn_confirmadjustment=fn_confirmadjustment
		else
			dim mg$(2)*255
			mg$(1) = "Warning: your balance adjustments for account "&gl$&" were NOT saved!"
			mg$(2) = "You can retry on the next screen, or modify another account."
			fnmsgbox(mat mg$,resp$,"Balance Adjustment Canceled",0)
			fn_confirmadjustment=0
		end if
fnend
include: Ertn
 
