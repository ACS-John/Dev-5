! load 'C:\ACS\Dev-5\Time Management\Update support expiration date.br'
fn_setup
fnTop(program$)

fn_updateSupportExpirationDate
goto Xit
def fn_updateSupportExpirationDate(; clientKey$*5)
	
	! r: open files
		open #hClientKey =fnH: "Name=S:\Core\Data\acsllc\Client.h420,Version=0,KFName=S:\Core\Data\acsllc\Client-Idx.h420,Use,RecL=534,KPs=1,KLn=5,Shr",internal,outIn,keyed
		open #hClientName=fnH: "Name=S:\Core\Data\acsllc\Client.h420,Version=0,KFName=S:\Core\Data\acsllc\CLIndx2.h420,Use,RecL=534,KPs=6,KLn=30,Shr",internal,outIn,keyed
		! CO Support
		open #hSupport   =fnH: "Name=S:\Core\Data\acsllc\Support.h420,Version=2,KFName=S:\Core\Data\acsllc\Support-Idx.h420,Use,RecL=246,KPs=1/7,KLn=6/2,Shr",internal,outIn,keyed
		F_support: form pos 1,C 6,n 2,c 2,n 8,c 2,n 8,n 10
	! /r	

	
	if clientKey$='' then ! r: ask client(s) and process them
		do
			Screen1: !
			fnToS
			alignRight=1
			lenCol1=10
			posCol2=lenCol1+2
			! (sfn$*100,lbuttonYNe,ps,width,df$*200,psk,lnk,psd,lnd; if$*200,limlis,urep,ttt$*200,contain,tabcon)
			fnLbl(1,1,'Client:',lenCol1,alignRight)
			fncombof('',1,posCol2,37,'S:\Core\Data\acsllc\Client.h[cno]',1,5,6,30,'S:\Core\Data\acsllc\Client-Idx.h[cno]',1)
			fnCmdSet( 2)
			fk=fnAcs(mat resp$)
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
				fnaddonec(mat msgConfirm$,'Update Expiration Dates? (adds one [timeframe] to each)')
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
	! r: close files
		close #hClientKey: ioerr ignore
		close #hClientName: ioerr ignore
		close #hSupport: ioerr ignore
		hClientKey=hClientName=hSupport=0
	! /r
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
	read #hSupport,using F_support,key=>rpad$(client$,kln(hSupport)): clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost
	do while rtrm$(client$)=rtrm$(clientId$)
		fnAddOneN(mat sysno,SysNo)
		fnaddoneC(mat sysid$,SysId$)
		fnAddOneN(mat dateStart,dateStart)
		fnaddoneC(mat timeFrame$,timeFrame$)
		fnAddOneN(mat dateExpire,dateExpire)
		fnAddOneN(mat cost,cost)
		read #hSupport,using F_support: clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost eof G1Finis
	loop
	G1Finis: !
 
fnend
def fn_updateOneSupportExpDate(client$)
	client$=rpad$(client$,kln(hSupport))
	read #hSupport,using F_support,key=>client$: clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost
	do while rtrm$(client$)=rtrm$(clientId$)
		! pr uCount+=1;rec(hSupport);clientId;SysNo;SysId$;timeFrame$,dateExpire;cost
		if timeFrame$='An' then
			dateExpireNew=val(str$(date(days(dateExpire,'ccyymmdd'),'ccyy')+1)&date$(days(dateExpire,'ccyymmdd'),'mmdd'))
		else if timeFrame$='Qt' then
			dateExpireNew=date(fnEndOfMonth(days(dateExpire,'ccyymmdd')+85),'ccyymmdd')
		else
			pr 'unrecognized time frame: '&timeFrame$
			pr ' please add code for newTimeFrame'
			pause
			end
			
		end if
		! pr 'dateExpireNew=';dateExpireNew
		! pause
		dateExpire=dateExpireNew
		rewrite #hSupport,using F_support: clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost
		read #hSupport,using F_support: client$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost eof U1Finis
	loop
	U1Finis: !
	! pause
fnend
def fn_setup
	autoLibrary
	dim resp$(10)*128
	tab$=chr$(9)
	buttonYN=4 : iconQuestion=32 : iconInformation=64 : buttonDefaultTwo=256
fnend
Xit: fnXit
include: ertn
