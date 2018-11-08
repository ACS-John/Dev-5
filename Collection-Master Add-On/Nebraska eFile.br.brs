fn_setup
tagTrigger$="NE-EFILE"
fn_testCmRead
fn_testNeEfile
goto Xit
def fn_testCmRead
	if ~setup_tag then
		setup_tag=1
		open #h_tags  :=fngethandle: "Name=Tags.int//6,kfname=Tags.idx//6,Shr",internal,outin,keyed
		open #hTagsClm:=fngethandle: "Name=Tags.int//6,kfname=Tags.clm//6,Shr",internal,outin,keyed
		open #hTagsCde:=fngethandle: "Name=Tags.int//6,kfname=Tags.cde//6,Shr",internal,outin,keyed
		open #hTagsDte:=fngethandle: "Name=Tags.int//6,kfname=Tags.dte//6,Shr",internal,outin,keyed
		dim tag$(0)*60,tagN(0),tag_fieldsc$(0)*20,tag_fieldsn$(0)*20,tag_formall$*512
		execute "*SubProc "&fnsql_setup$('Tags',mat tag$,mat tagN,mat tag_fieldsc$,mat tag_fieldsn$,tag_formall$)
		! let h_tags=fnopen_sql_file("tags",openclosed$) ! kps=1,9,13  kln=8,4,4
		open #hClaim:=fngethandle: 'name=master//6,kfname=masterx//6,shr',internal,outin,keyed
		dim claim$(0)*128,claimN(0),claim_fieldsc$(0)*20,claim_fieldsn$(0)*20,claim_formall$*2048
		execute "*SubProc "&fnsql_setup$('Master',mat claim$,mat claimN,mat claim_fieldsc$,mat claim_fieldsn$,claim_formall$)
	end if

	restore #hTagsCde,key=>rpad$(tagTrigger$,kln(hTagsCde)):
	do
		read #hTagsCde,using tag_formall$: mat tag$,mat tagN eof EoTags
		tagReadCount+=1
		if tagTrigger$=rtrm$(tag$(tags_code)) then
			read #hClaim,using claim_formall$,key=tag$(tags_fileno): mat claim$,mat claimN
			pr '  master.fileno='&claim$(master_fileno)
		end if
	loop while tagTrigger$=rtrm$(tag$(tags_code))
	EoTags: !
	pr 'tagReadCount=';tagReadCount
	pause
	
	close #h_tags  :
	close #hTagsClm:
	close #hTagsCde:
	close #hTagsDte:
	setup_tag=0
	close #hClaim:
fnend
GetBitCoinPriceTest: ! r:
	for _price=1 to 20
		print "Bitcoin Price:"&date$("MM/DD/CCYY")&"-"&time$;fn_getBtcPrice
	sleep(1)
	next _price
return ! /r
Xit: end

def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library.br': fngethandle
		library "SQL/Library": fnsql_setup,fnsql_id_parse,fnsql_setup$,fnopen_sql_file
	end if
fnend
def fn_getBtcPrice(; ___,hConnection,Market$,string$*999)
	open #hConnection:=1: "name= https://min-api.cryptocompare.com/data/price?fsym=BTC&tsyms=USD"&Market$&"&extraParams=SageLive, http=client",DISPLAY,OUTIN
	linput #hConnection: string$ eof Ignore
	string$(1:pos(string$,"usd")+4)=""
	string$=srep$(string$,"}","")
	fn_getBtcPrice=val(string$) conv Ignore
	close #hConnection:
fnend
def fn_testNeEfile(; ___,hConnection,string$*999,returnN)
	open #hConnection:=2: "name=https://ne-test.cdc.nicusa.com/apps-EFILE/services/createFiling?wsdl, http=client",DISPLAY,OUTIN
	dim resultFile$*256
	resultFile$=env$('temp')&'\testNeEfileReturn.xml'
	open #hResult    :=3: 'name='&resultFile$&',recl=1024,replace',d,o
	! pr #hConnection: 'listFilers();'
	fn_soapEncapsulateSimple(hConnection,'listFilers();')
	! pr #hConnection: 'CreateCriminalResponse createNewCriminalCase('  !	CreateCriminalResponse createNewCriminalCase(
	! pr #hConnection: '	String court,                     '  !		String court,
	! pr #hConnection: '	01,                               '  !		Integer county,
	! pr #hConnection: '	MISD,                             '  !		String caseSubType,
	! pr #hConnection: '	2018-10-15,                       '  !		Date offenseDate,
	! pr #hConnection: '	2018-10-20,                       '  !		Date hearingDate,
	! pr #hConnection: '	CI123,                            '  !		String citationNumber,
	! pr #hConnection: '	10892,                            '  !		Integer prosecutorBarNumber,  (1-2-3-1 Attorney - Bar Number)
	! pr #hConnection: '	String filedBy,                   '  !		String filedBy,
	! pr #hConnection: '	String notificationEmail,         '  !		String notificationEmail,
	! pr #hConnection: '	String optNotificationEmail1,     '  !		String optNotificationEmail1,
	! pr #hConnection: '	String optNotificationEmail2,     '  !		String optNotificationEmail2,
	! pr #hConnection: '	String custMemo,                  '  !		String custMemo,
	! pr #hConnection: '	Integer dssNumber                 '  !		Integer dssNumber
	! pr #hConnection: ');                                 '  !	);

	do
		linput #hConnection: string$ eof EoTnE
		pr #hResult: string$
	loop
	EoTnE: !
	close #hConnection:
	exe 'sy -c "'&resultFile$&'"'
	fn_testNeEfile=returnN
fnend
def fn_soapEncapsulateSimple(hX,text$*2048)
pr #hX: 'POST /ws/documents HTTP/1.1'
pr #hX: '...'
pr #hX: 'Content-Type: text/xml; charset=utf-8'
pr #hX: 'Content-Length: 400'
pr #hX: '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">'
pr #hX: '  <SOAP-ENV:Header />'
pr #hX: '  <SOAP-ENV:Body>'
! pr #hX: '    <ns2:storeDocumentRequest xmlns:ns2="https://github.com/ralfstuckert/mtom">'
! pr #hX: '      <ns2:document>'
! pr #hX: '        <ns2:name>30</ns2:name>'
! pr #hX: '        <ns2:author>John Bowman</ns2:author>'
pr #hX: '        <ns2:content>'&text$&'</ns2:content>'
! pr #hX: '      </ns2:document>'
! pr #hX: '    </ns2:storeDocumentRequest>'
pr #hX: '  </SOAP-ENV:Body>'
pr #hX: '</SOAP-ENV:Envelope>'
fnend
def fn_AddWsseSecurityHeader(hX,username$*256,password$*256)
pr #hX: '<Security xmlns="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">'
pr #hX: '  <UsernameToken>'
pr #hX: '    <Username>'&username$&'</Username>'
pr #hX: '    <Password>'&password$&'</Password>'
pr #hX: '  </UsernameToken>'
pr #hX: '</Security>'
fnend
