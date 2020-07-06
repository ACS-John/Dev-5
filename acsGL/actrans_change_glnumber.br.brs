! Replace S:\acsGL\actrans_change_glnumber
 
	autoLibrary
	on error goto Ertn
 
	dim cnam$*40,cap$*128,resp$(100)*60
	dim balance_current_year_month(13),balance_prior_year_month(13),rf(6)
	dim actrans_key$*20
 
	fncno(cno,cnam$)
	fnTop(program$, cap$="Change GL Numbers in ACTrans")
 
	gln_from$=' 12   101  0' : gln_to$='  1   101  0'
	if fn_screen_1(gln_from$,gln_to$)=5 then goto Xit
	fn_report(cap$)
	fn_report(date$('mm/dd/ccyy'))
	fn_report('')
	open #h_actrans:=fngethandle: "Name=[Q]\GLmstr\AcTrans.H[cno],KFName=[Q]\GLmstr\AcTrIdx.H[cno],Shr",internal,outIn,keyed
F_ACTRANS: form pos 1,c 12,n 6,pd 6.2,n 2,pos 71,n 2
	restore #h_actrans,key>=rpad$(gln_from$,kln(h_actrans)):
	do
		read #h_actrans,using F_ACTRANS: gl$
		if gl$>gln_from$ then goto EO_ACTRANS
		if gl$=gln_from$ then
			fn_report('rec '&str$(rec(h_actrans)))
			gl$=gln_to$
			rewrite #h_actrans,using F_ACTRANS: gl$
		end if  ! gln_period_did_change>0
	loop
EO_ACTRANS: !
	fncloseprn : report_open=0
Xit: fnXit
 
def fn_screen_1(&gln_from$,&gln_to$)
		fnTos(sn$="FixGLN")
		mylen=22
		mypos=mylen+2
		respc=0 : myline=0
		fnLbl(myline+=1,1,"Change GL Number From:",mylen,1)
		fnTxt(myline,mypos,12,0,1)
		resp$(respc+=1)=gln_from$
		fnLbl(myline+=1,1,"To:",mylen,1)
		fnTxt(myline,mypos,12,0,1)
		resp$(respc+=1)=gln_to$
		fnCmdSet(2)
		fnAcs(mat resp$,ckey)
		if ckey<>5 then
			gln_from$=lpad$(resp$(1),12)
			gln_to$=lpad$(resp$(2),12)
		end if  ! ckey<>5 then
		fn_screen_1=ckey
fnend  ! fn_screen_1
def fn_report(line$*256)
		if ~report_open then
			report_open=1
			fnopenprn
		end if  ! ~report_open
		pr #255: line$ ! if gl$='  6   101  0' then pr #255: line$
fnend
 
include: Ertn
