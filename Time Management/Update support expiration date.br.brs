! load 'C:\ACS\Dev-5\Time Management\Update support expiration date.br'
fn_setup
fn_openFiles
fnTop(program$)
fn_updateSupportExpirationDate
goto XIT
def fn_updateSupportExpirationDate(; clientKey$*5)
	if clientKey$='' then ! r: ask client(s) and process them
		do
			Screen1: !
			fnToS
			alignRight=1
			lenCol1=10
			posCol2=lenCol1+2
			! (sfn$*100,lbuttonYNe,ps,width,df$*200,psk,lnk,psd,lnd; if$*200,limlis,urep,ttt$*200,contain,tabcon)
			fnLbl(1,1,'Client:',lenCol1,alignRight)
			fncombof('',1,posCol2,37,'[Q]\TMmstr\CLmstr.H[cno]',1,5,6,30,'[Q]\TMmstr\CLIndex.H[cno]',1)
			fnCmdSet( 2)
			fnAcs('',0,mat resp$, fk)
			if fk<>5 then 
				dim selectedClient$*128
				selectedClient$=resp$(1)
				clientKey$=selectedClient$(1:5)
				dim msgConfirm$(0)*256
				mat msgConfirm$(0)
				fnaddonec(mat msgConfirm$,'Client:'&tab$&selectedClient$)
				fnaddonec(mat msgConfirm$,'')
				fn_addSupportMsgLines$(mat msgConfirm$)
				fnaddonec(mat msgConfirm$,'')
				fnaddonec(mat msgConfirm$,'Total Cost:'&tab$&tab$&cnvrt$('pic($$$,$$#.##)',sum(mat cost)))
				fnaddonec(mat msgConfirm$,'')
				fnaddonec(mat msgConfirm$,'Update Expiration Dates? (adds one year to each)')
				fnmsgbox(mat msgConfirm$, resp$,'',buttonYN+iconQuestion+buttonDefaultTwo)
				if resp$='Yes' then 
					fn_updateOneSupportExpDate(selectedClient$(1:5))
					mat msgConfirm$(0)
					fnaddonec(mat msgConfirm$,'Client:'&tab$&selectedClient$)
					fnaddonec(mat msgConfirm$,'')
					fn_addSupportMsgLines$(mat msgConfirm$)
					fnaddonec(mat msgConfirm$,'')
					fnaddonec(mat msgConfirm$,'Total Cost:'&tab$&tab$&cnvrt$('pic($$$,$$#.##)',sum(mat cost)))
					fnaddonec(mat msgConfirm$,'')
					fnaddonec(mat msgConfirm$,'UPDATE COMPLETED')
					fnmsgbox(mat msgConfirm$,resp$,'',0+iconInformation)
				end if
			end if
		loop until fk=5
		! /r
	else !  just process the client passed
		fn_updateOneSupportExpDate(clientKey$)
	end if
	! usedXit: !
fnend
def fn_addSupportMsgLines$(mat msgText$)
	fn_getSupportArrayByClient(clientKey$,mat sysno,mat sysid$,mat dateStart,mat timeFrame$,mat dateExpire,mat cost)
	for supItem=1 to udim(mat sysno)
		fnaddonec(mat msgText$,rpad$(str$(sysno(supItem)),2)&'. '&sysid$(supItem)&tab$&timeFrame$(supItem)&' '&date$(days(dateExpire(supItem),'ccyymmdd'),'mm/dd/ccyy')&tab$&cnvrt$('pic(zzz,zzz.zz)',cost(supItem)))
	nex supItem
fnend
def fn_getSupportArrayByClient(client$,mat sysno,mat sysid$,mat dateStart,mat timeFrame$,mat dateExpire,mat cost)
	mat sysno(0)
	mat sysid$(0)
	mat dateStart(0)
	mat timeFrame$(0)
	mat dateExpire(0)
	mat cost(0)
	read #hSupport,using fSupport,key=>rpad$(client$,kln(hSupport)): clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost
	do while rtrm$(client$)=rtrm$(clientId$)
		fnAddOneN(mat sysno,SysNo)
		fnaddoneC(mat sysid$,SysId$)
		fnAddOneN(mat dateStart,dateStart)
		fnaddoneC(mat timeFrame$,timeFrame$)
		fnAddOneN(mat dateExpire,dateExpire)
		fnAddOneN(mat cost,cost)
		read #hSupport,using fSupport: clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost eof G1Finis
	loop
	G1Finis: ! 

fnend
def fn_updateOneSupportExpDate(client$)
	client$=rpad$(client$,kln(hSupport))
	read #hSupport,using fSupport,key=>client$: clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost
	do while rtrm$(client$)=rtrm$(clientId$)
		! pr uCount+=1;rec(hSupport);clientId;SysNo;SysId$;timeFrame$,dateExpire;cost
		dateExpireNew=val(str$(date(days(dateExpire,'ccyymmdd'),'ccyy')+1)&date$(days(dateExpire,'ccyymmdd'),'mmdd'))
		! pr 'dateExpireNew=';dateExpireNew
		dateExpire=dateExpireNew
		rewrite #hSupport,using fSupport: clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost
		read #hSupport,using fSupport: client$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost eof U1Finis
	loop
	U1Finis: ! 
	! pause
fnend
def fn_openFiles
	if ~openFiles then
		openFiles=1
		open #hClientKey :=fngethandle: "Name=[Q]\TMmstr\CLmstr.h420,Version=0,KFName=[Q]\TMmstr\CLIndex.h420,Use,RecL=534,KPs=1,KLn=5,Shr",internal,outIn,keyed 
		open #hClientName:=fngethandle: "Name=[Q]\TMmstr\CLmstr.h420,Version=0,KFName=[Q]\TMmstr\CLIndx2-Idx.h420,Use,RecL=534,KPs=6,KLn=28,Shr",internal,outIn,keyed 
		open #hSupport   :=fngethandle: "Name=[Q]\TMmstr\Support.h420,Version=2,KFName=[Q]\TMmstr\Support-Idx.h420,Use,RecL=246,KPs=1/7,KLn=6/2,Shr",internal,outIn,keyed
	end if
	fSupport: form pos 1,C 6,n 2,c 2,n 8,c 2,n 8,n 10
fnend
def fn_setup
	library 'S:\Core\Library': fntop,fnxit, fnerror
	library 'S:\Core\Library': fngethandle,fnaddonec,fnAddOneN
	library 'S:\Core\Library': fnmsgbox
	library 'S:\Core\Library': fnAcs,fnTos,fnLbl,fncombof,fnCmdSet
	dim resp$(10)*128
	tab$=chr$(9)
	buttonYN=4 : iconQuestion=32 : iconInformation=64 : buttonDefaultTwo=256 
fnend
XIT: fnxit
include: ertn
