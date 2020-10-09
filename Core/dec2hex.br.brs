! Replace S:\Core\Dec2Hex.br

def library fndec2hex(input_dec,&output_hex$)
	autoLibrary
	on error goto Ertn

	dim d2h_temp$*1024

	d2h_temp$=""
	for i=8 to 0 step -1
		d=int(input_dec/(16**i))
		if d>9 then : _
			d2h_temp$(99:0)=chr$(int(d)+55) else : _
			d2h_temp$(99:0)=str$(int(d))
		input_dec=input_dec-d*(16**i)
	next i
	d2h_temp$=ltrm$(d2h_temp$)
	if d2h_temp$="" then output_hex$="0" else : _
		output_hex$=d2h_temp$
	goto Xit
Xit: fnend
include: ertn

