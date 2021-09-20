! transactions explorer
fn_setup
dim tr$(0)*128
dim trN(0)
hTrans=fn_openFio('TM Transaction',mat tr$,mat trN, 1)
dim coClient$*40
coClient$='[All]'
coProvider$='[All]'
providerOpt$(1)='ACS'
providerOpt$(1)='CSS'
providerOpt$(1)='JB'
do
	fntos : rc=0
	dim resp$(16)*256
	col1pos= 2
	col1len=20
	col2pos=col1len+col1pos+2
	fnLbl(ln+=1,1,'Client:', col1len,1)
	! fnComboFio(lyne,ps,layoutName$*128, whichIndex,limitToListorAddAll)
	fnComboFio(ln,col2pos,'TM Client 420', 2)
	resp$(resp_client=rc+=1)=coClient$
	
	! fnComboA(ln,col2pos,'TM Provider', 2)
	fnLbl(ln+=1,1,'Provider:', col1len,1)
	fnComboA('provider',ln,col2pos,mat providerOpt$) ! , ttt$*200,width,contain,tabcon)
	fnButton(ln+=1,(val(env$('Session_Cols'))-8),'Refresh',ck_refresh=19, 'reapply the filters')
	
	ln+=1
	! r: add grid
		dim ch$(10)

		ch$(1 )='Client'            	: colMask$(1 )=''
		ch$(2 )='Invoice'           	: colMask$(2 )=''
		ch$(3 )='Date'              	: colMask$(3 )='1'
		ch$(4 )='Origional Amt'     	: colMask$(4 )=''
		ch$(5 )='Amount'            	: colMask$(5 )=''
		ch$(6 )='Salesman'          	: colMask$(6 )=''
		ch$(7 )='Transaction Type' 	: colMask$(7 )=''
		ch$(8 )='PC'                	: colMask$(8 )=''
		ch$(9 )='Inv Description'  	: colMask$(9 )=''
		ch$(10)='NTA'                	: colMask$(10)=''

		fnFlexInit1('explorer',ln+=1,col1pos,10,10,mat ch$, mat colMask$)

		do
			read #hTrans,using form$(hTrans): mat tr$,mat trN eof TransGridAddEoF
			if coClient$='[All]' or trim$(coClient$)=trim$(tr$(tr_clientId)) then
				! r: add record to grid
				dim item$(10)*128
				mat item$(10)

				mat item$=('')
				item$(1 )=     tr$(tr_clientId     	)
				item$(2 )=     tr$(tr_inv          	)
				item$(3 )=str$(trN(tr_date         	))
				item$(4 )=str$(trN(tr_amtOrigional	))
				item$(5 )=str$(trN(tr_amt          	))
				item$(6 )=str$(trN(tr_salesmanId  	))
				item$(7 )=str$(trN(tr_transCode   	))
				item$(8 )=str$(trN(tr_postCode    	))
				item$(9 )=     tr$(tr_desc         	)
				item$(10)=str$(tnN(tr_nta          	))
				fnFlexAdd1(mat item$)
				! /r
			end if
		loop
		TransGridAddEoF: !
	! /r
	! fncmdset(14)
	fnCmdKey("Add"   	,ck_add   =1)
	fnCmdKey("Edit"  	,ck_edit  =2,1)
	fnCmdKey("Delete"	,ck_delete=4)
	fnCmdKey("Exit"  	,ck_cancel=5,0,1)
	ckey=fnacs(mat resp$)
	if ckey<>ck_cancel then
		coClient$=resp$(resp_client)
		if ckey=ck_add then
		else if ckey=ck_edit then
			pr 'ck_edit' : pause
		else if ckey=ck_delete then
			pr 'ck_delete' : pause
		! else if ckey=ck_refresh then ! nothing needs to be done, just loop
		! 	pr 'ck_refresh' : pause
		end if
	end if
loop

Xit: fnXit
include: fn_open
include: fn_setup