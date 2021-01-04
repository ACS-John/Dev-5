print newpage

autoLibrary

for repeat=1 to 5
	print 'Bitcoin Price as of '&date$("MM/DD/CCYY")&' - '&time$&' is '&str$(fn_getPrice)
	sleep(1)
next repeat

end

def fn_getPrice(;___,hConnection,line$*999)
	open #hConnection=fnH: "name= https://min-api.cryptocompare.com/data/price?fsym=BTC&tsyms=USD&extraParams=SageLive, http=client",DISPLAY,OUTIN
	linput #hConnection: line$ eof Ignore
	line$(1:pos(line$,'USD')+4)=""
	line$=srep$(line$,"}","")
	fn_getPrice=val(line$) conv Ignore
	close #hConnection:
fnend 