fn_setup
tagTrigger$="NE-EFILE"
! fn_testCmRead
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
	on error goto Ertn
fnend
Ertn: ! r:
if err=4340 then 
	pr 'BR! Err 4340 SysErr='&str$(syserr)&' - '&syserr$
	pause
else
	pr 'error '&str$(err)&' on line '&str$(line)&' of '&os_Filename$(program$)
	pause
	retry
end if
! /r
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
	! ! old old guess  !!!  pr #hConnection: 'CreateCriminalResponse createNewCriminalCase('  !	CreateCriminalResponse createNewCriminalCase(
	! ! old old guess  !!!  pr #hConnection: '	String court,                     '  !		String court,
	! ! old old guess  !!!  pr #hConnection: '	01,                               '  !		Integer county,
	! ! old old guess  !!!  pr #hConnection: '	MISD,                             '  !		String caseSubType,
	! ! old old guess  !!!  pr #hConnection: '	2018-10-15,                       '  !		Date offenseDate,
	! ! old old guess  !!!  pr #hConnection: '	2018-10-20,                       '  !		Date hearingDate,
	! ! old old guess  !!!  pr #hConnection: '	CI123,                            '  !		String citationNumber,
	! ! old old guess  !!!  pr #hConnection: '	10892,                            '  !		Integer prosecutorBarNumber,  (1-2-3-1 Attorney - Bar Number)
	! ! old old guess  !!!  pr #hConnection: '	String filedBy,                   '  !		String filedBy,
	! ! old old guess  !!!  pr #hConnection: '	String notificationEmail,         '  !		String notificationEmail,
	! ! old old guess  !!!  pr #hConnection: '	String optNotificationEmail1,     '  !		String optNotificationEmail1,
	! ! old old guess  !!!  pr #hConnection: '	String optNotificationEmail2,     '  !		String optNotificationEmail2,
	! ! old old guess  !!!  pr #hConnection: '	String custMemo,                  '  !		String custMemo,
	! ! old old guess  !!!  pr #hConnection: '	Integer dssNumber                 '  !		Integer dssNumber
	! ! old old guess  !!!  pr #hConnection: ');                                 '  !	);

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
! old guess  !!!   pr #hX: 'POST /ws/documents HTTP/1.1'
! old guess  !!!   pr #hX: '...'
! old guess  !!!   pr #hX: 'Content-Type: text/xml; charset=utf-8'
! old guess  !!!   pr #hX: 'Content-Length: 400'
! old guess  !!!   pr #hX: '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">'
! old guess  !!!   pr #hX: '  <SOAP-ENV:Header />'
! old guess  !!!   pr #hX: '  <SOAP-ENV:Body>'
! old guess  !!!   ! pr #hX: '    <ns2:storeDocumentRequest xmlns:ns2="https://github.com/ralfstuckert/mtom">'
! old guess  !!!   ! pr #hX: '      <ns2:document>'
! old guess  !!!   ! pr #hX: '        <ns2:name>30</ns2:name>'
! old guess  !!!   ! pr #hX: '        <ns2:author>John Bowman</ns2:author>'
! old guess  !!!   pr #hX: '        <ns2:content>'&text$&'</ns2:content>'
! old guess  !!!   ! pr #hX: '      </ns2:document>'
! old guess  !!!   ! pr #hX: '    </ns2:storeDocumentRequest>'
! old guess  !!!   pr #hX: '  </SOAP-ENV:Body>'
! old guess  !!!   pr #hX: '</SOAP-ENV:Envelope>'







dim wsseUsername$*128
wsseUsername$='bqlaw05'
dim wssePassword$*128
wssePassword$='UHtHXXtT'

pr #hX: 'POST https://ne-test.cdc.nicusa.com/apps-EFILE/services/createFiling HTTP/1.1'
pr #hX: 'Accept-Encoding: gzip,deflate'
pr #hX: 'Content-Type: text/xml;charset=UTF-8'
pr #hX: 'SOAPAction: ""'
pr #hX: 'Content-Length: 1413'
pr #hX: 'Host: ne-test.cdc.nicusa.com'
pr #hX: 'Connection: Keep-Alive'
pr #hX: 'User-Agent: Apache-HttpClient/4.1.1 (java 1.5)'
pr #hX: ''
pr #hX: '<soapenv:Envelope xmlns:efil="http://efile.justice.courts.ne.nicusa.com/" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">'
pr #hX: '   <soapenv:Header>';
pr #hX:      '<wsse:Security xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd" xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">';
pr #hX:      '<wsse:UsernameToken wsu:Id="UsernameToken-9FC6E4CABDD2AB2C1215433321371302">';
pr #hX:      '<wsse:Username>'&wsseUsername$&'</wsse:Username>';
pr #hX:      '<wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText">'&wssePassword$&'</wsse:Password>';
pr #hX:      '<wsse:Nonce EncodingType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary">lgTyBOfJ+eg3V/Y2fdpE5A==</wsse:Nonce>';
pr #hX:      '<wsu:Created>'&date$('ccyy-mm-dd')&'T'&time$&'.129Z</wsu:Created>';
pr #hX:      '</wsse:UsernameToken>';
pr #hX:      '</wsse:Security>';
pr #hX:      '</soapenv:Header>'

pr #hX: '   <soapenv:Body>'
pr #hX: '      <efil:loadExistingCase>'
pr #hX: '         <caseNumber>C99CI150000004</caseNumber>'
pr #hX: '         <filerBarNumber>10151</filerBarNumber>'
pr #hX: '         <!--Optional:-->'
pr #hX: '         <hearingDate>2018-11-05T13:00:00Z</hearingDate>'
pr #hX: '         <notificationEmail>dave@egov.com</notificationEmail>'
pr #hX: '         <!--Optional:-->'
pr #hX: '         <optionalNotificationEmail1>dpfister@egov.com</optionalNotificationEmail1>'
pr #hX: '         <custMemo>This is a sample customer memo</custMemo>'
pr #hX: '      </efil:loadExistingCase>'
pr #hX: '   </soapenv:Body>'
pr #hX: '</soapenv:Envelope>'








fnend
def fn_AddWsseSecurityHeader(hX,username$*256,password$*256)
pr #hX: '<Security xmlns="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">'
pr #hX: '  <UsernameToken>'
pr #hX: '    <Username>'&username$&'</Username>'
pr #hX: '    <Password>'&password$&'</Password>'
pr #hX: '  </UsernameToken>'
pr #hX: '</Security>'
fnend
