fn_setup

fn_testNeEfile

goto Xit

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
		library 's:\Core\Library': fngethandle
	end if
fnend
def fn_getBtcPrice(; ___,hConnection,Asofdate$,P,Market$,string$*999)
	open #hConnection:=fngethandle: "name= https://min-api.cryptocompare.com/data/price?fsym=BTC&tsyms=USD"&Market$&"&extraParams=SageLive, http=client",DISPLAY,OUTIN
	linput #hConnection: string$ eof Ignore
	string$(1:pos(string$,"usd")+4)=""
	string$=srep$(string$,"}","")
	fn_getprice=val(string$) conv Ignore
	close #hConnection:
fnend
def fn_testNeEfile(; ___,hConnection,Asofdate$,P,Market$,string$*999)
	open #hConnection:=fngethandle: "name=https://ne-test.cdc.nicusa.com/apps-EFILE/services/createFiling?wsdl, http=client",DISPLAY,OUTIN
	linput #hConnection: string$ eof Ignore
	string$(1:pos(string$,"usd")+4)=""
	string$=srep$(string$,"}","")
	fn_testNeEfile=val(string$) conv Ignore
	close #hConnection:
fnend
