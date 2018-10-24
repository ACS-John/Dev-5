fn_setup
fn_testCmRead
fn_testNeEfile

goto Xit
def fn_testCmRead
	! C:\Brumbaugh\clsinc\PROG2\mast2.wb.brs
 open #hMaster:=1: "name=C:\Brumbaugh\clsinc\DATA\MASTER,kfname=C:\Brumbaugh\clsinc\DATA\MASTERX,shr",internal,outin,keyed
 pause
 close #hMaster:
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
		! library 's:\Core\Library': fngethandle
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
