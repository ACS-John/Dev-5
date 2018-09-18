print newpage
dim string$*999,market$*999
library "": fngetprice

for _price=1 to 20
	print "Bitcoin Price:"&date$("MM/DD/CCYY")&"-"&time$;fngetprice("BTC",0,Time$,"")
sleep(1)
next _price

stop

def library fngetprice(;Currencytype$,Asofdate,Asoftime$,Exchange$,___,Connection,Asofdate$,P,Market$)
	IF ~Len(Currencytype$) then Currencytype$="BTC" ! BTC
	! fnLibraryLinkage
	IF Asofdate then
		if len(trim$(exchange$)) then
			market$="&markets="&exchange$
		end if
		open #(connection:=fngetfilenumber): "name= https://min-api.cryptocompare.com/data/pricehistorical?fsym="&Currencytype$&"&tsyms=USD&ts="&Str$(Fnepochtime(Asofdate,Asoftime$))&Market$&"&extraParams=SageLive, http=client",DISPLAY,OUTIN
	else
		if len(trim$(exchange$)) then
			market$="&e="&exchange$
		end if
		open #(connection:=fngetfilenumber): "name= https://min-api.cryptocompare.com/data/price?fsym="&Currencytype$&"&tsyms=USD"&Market$&"&extraParams=SageLive, http=client",DISPLAY,OUTIN
	end if
	linput #connection: string$ eof Ignore
	string$(1:pos(string$,"usd")+4)=""
	string$=srep$(string$,"}","")
	fngetprice=val(string$) conv Ignore
	close #connection:
fnend

def fngetfilenumber
	_next_file=1000
	do while file(_next_file-=1)<>-1
	loop 
	fngetfilenumber=_next_file
fnend 

def fnepochtime(et_date,et_time$)
	fnepochtime=0
fnend 
Ignore: continue 