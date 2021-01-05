print newpage

autoLibrary

! for repeat=1 to 5
	print 'Bitcoin Price as of '&date$("MM/DD/CCYY")&' - '&time$&' is '&str$(fn_getPrice)
	sleep(1)
! next repeat

open #hC=fnH: "name= http://planetacs.net/acs5update/Release_Notes.txt, http=client",display,outin
do
	dim line$*2048
	linput #hC: line$ eof EoC
	lineCount+=1
	pr line$
	if lineCount/24=int(lineCount/24) then pause
loop
EoC: !
close #hC:

end

def fn_getPrice(;___,hConnection,line$*999)
	open #hConnection=fnH: "name= https://min-api.cryptocompare.com/data/price?fsym=BTC&tsyms=USD&extraParams=SageLive, http=client",display,outin
	linput #hConnection: line$ eof ignore
	line$(1:pos(line$,'USD')+4)=""
	line$=srep$(line$,"}","")
	fn_getPrice=val(line$) conv ignore
	close #hConnection:
fnend
